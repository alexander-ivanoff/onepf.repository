package org.onepf.repository;

import org.onepf.repository.model.GetPurchasesRequestHandler;
import org.onepf.repository.model.services.DataException;
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
    private static final String PARAMETER_PAGE = "page";

    public final static String FILE_PREFIX = "purchases";
    private final static String FILE_TEMPLATE = FILE_PREFIX + "_%s_%d.xml";

    private GetPurchasesRequestHandler getPurchasesRequestHandler;

    @Override
    public void init() throws ServletException {
        super.init();
        getPurchasesRequestHandler = getRepositoryFactory().createPurchasesHandler();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        String packageName = request.getParameter(PARAMETER_PACKAGE);
        if (packageName == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "No package defined");
            return;
        }

        try {
            String page = request.getParameter(PARAMETER_PAGE);
            List<PurchaseDescriptor> purchases = getPurchasesRequestHandler.getPurchases(packageName, page != null? Integer.valueOf(page) : -1);
            String prevFileLink = null;
            String lastUpdate = null;
            if (purchases.size() > 0 ) {
                PurchaseDescriptor lastPurchase = purchases.get(0);
                lastUpdate = lastPurchase.lastUpdate;
                if (lastPurchase.currPageHash != lastPurchase.prevPageHash) {
                    prevFileLink = String.format(FILE_TEMPLATE, packageName, lastPurchase.prevPageHash);
                }
            }
            ResponseWriter responseWriter = new XmlResponseWriter();
            responseWriter.writePurchases(response.getWriter(), purchases, lastUpdate, prevFileLink);
        } catch (WriteException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
