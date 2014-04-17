package org.onepf.repository;

import org.onepf.repository.api.responsewriter.entity.PurchaseEntity;
import org.onepf.repository.model.GetPurchasesRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.api.responsewriter.ResponseWriter;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.XmlResponseWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 *
 * This Servlet returns list of purchases for all packages provided by requesting appstore.
 *
* @author Alexander Ivanoff on 11.03.14.
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
            List<PurchaseEntity> purchases = getPurchasesRequestHandler.getPurchases(packageName, page != null? Integer.valueOf(page) : -1);
            String prevOffset = null;
            String lastUpdate = null;
            if (purchases.size() > 0 ) {
                PurchaseEntity lastPurchase = purchases.get(0);
                lastUpdate = lastPurchase.getLastUpdate();
                if (lastPurchase.getCurrPageHash() != lastPurchase.getPrevPageHash()) {
                    prevOffset = String.format(FILE_TEMPLATE, packageName, lastPurchase.getPrevPageHash());
                }
            }
            ResponseWriter responseWriter = new XmlResponseWriter();
            responseWriter.writePurchases(response.getWriter(), purchases, prevOffset);
        } catch (WriteException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
