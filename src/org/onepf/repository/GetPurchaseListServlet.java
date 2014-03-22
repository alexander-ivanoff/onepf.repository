package org.onepf.repository;

import org.onepf.repository.model.Options;
import org.onepf.repository.model.PurchasesList;
import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.WriteException;
import org.onepf.repository.utils.responsewriter.XmlResponseWriter;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * Created by ivanoff on 11.03.14.
 */
public class GetPurchaseListServlet extends BaseServlet {

    private static final String PARAMETER_PACKAGE = "package";
    private static final String PARAMETER_DATE = "date";

    private PurchasesList purchasesList;

    @Override
    public void init() throws ServletException {
        super.init();
        purchasesList = getRepositoryFactory().createPurchasesList();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        String packageName = request.getParameter(PARAMETER_PACKAGE);
        if (packageName == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "No package defined");
            return;
        }

        // Date is optional parameter. Can be null
        String date = request.getParameter(PARAMETER_DATE);
        if (date != null ) {
            // TODO PARSE DATE
        }

        Options options = new Options(2);
        options.put(Options.PACKAGE_NAME, packageName);
        options.put(Options.UPDATE_TIME, String.valueOf(0));

        List<PurchaseDescriptor> purchases = purchasesList.getPurchases(options);
        ResponseWriter responseWriter = new XmlResponseWriter();
        try {
            responseWriter.writePurchases(response.getWriter(), purchases);
        } catch (WriteException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
