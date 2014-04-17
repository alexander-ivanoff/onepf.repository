package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseWriterV2;
import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.api.xmlapi.XmlResponseWriterV2;
import org.onepf.repository.model.GetPurchasesRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.api.responsewriter.WriteException;

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

    private static final String PARAMETER_PAGE = "page";

    public final static String FILE_PREFIX = "purchases";
    private final static String FILE_TEMPLATE = FILE_PREFIX + "_%d.xml";

    private static final ResponseWriterV2 responseWriter = initResponseWriter();

    private static ResponseWriterV2 initResponseWriter() {
        XmlResponseWriterV2 responseWriter = null;
        try {
            responseWriter = new XmlResponseWriterV2(ObjectFactory._Downloads_QNAME, DownloadListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return responseWriter;
    }

    private GetPurchasesRequestHandler getPurchasesRequestHandler;

    @Override
    public void init() throws ServletException {
        super.init();
        getPurchasesRequestHandler = getRepositoryFactory().createPurchasesHandler();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        try {
            String page = request.getParameter(PARAMETER_PAGE);
            List<PurchaseEntity> purchases = getPurchasesRequestHandler.getPurchases(getAppstore(request).appstoreId, page != null? Integer.valueOf(page) : -1);
            String offset = getOffset(request, purchases);
            responseWriter.write(response.getOutputStream(), buildListEntity("1", offset, purchases));
        } catch (WriteException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    private static String getOffset(HttpServletRequest request, List<PurchaseEntity> purchases) {
        String offset = null;
        if (purchases.size() > 0 ) {
            PurchaseEntity lastPurchase = purchases.get(0);
            if (lastPurchase.getCurrPageHash() != lastPurchase.getPrevPageHash()) {
                offset = String.format(FILE_TEMPLATE, lastPurchase.getPrevPageHash());
            }
        }
        return offset;
    }

    private static PurchaseListEntity buildListEntity(String version, String offset, List<PurchaseEntity> downloads) {
        PurchaseListEntity listEntity = new PurchaseListEntity();
        listEntity.setVersion(version);
        listEntity.setOffset(offset);
        listEntity.getPurchase().addAll(downloads);
        return listEntity;
    }

}
