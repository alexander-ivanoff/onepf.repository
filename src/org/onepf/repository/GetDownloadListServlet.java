package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseWriterV2;
import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.api.xmlapi.XmlResponseWriterV2;
import org.onepf.repository.model.GetDownloadsRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.api.responsewriter.WriteException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 *
 *  This Servlet returns handler of downloads for all packages provided by requesting appstore.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class GetDownloadListServlet extends BaseServlet {

    private static final String PARAMETER_PAGE = "page";

    public final static String FILE_PREFIX = "downloads";
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

    private GetDownloadsRequestHandler handler;

    @Override
    public void init() throws ServletException {
        super.init();
        handler = getRepositoryFactory().createDownloadsHandler();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        try {
            String page = request.getParameter(PARAMETER_PAGE);
            List<DownloadEntity> downloads = handler.getDownloads(getAppstore(request).appstoreId, page != null? Integer.valueOf(page) : -1);
            String offset = getOffset(request, downloads);
            responseWriter.write(response.getOutputStream(), buildListEntity("1", offset, downloads));
        } catch (WriteException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    private static String getOffset(HttpServletRequest request, List<DownloadEntity> downloads) {
        String offset = null;
        if (downloads.size() > 0 ) {
            DownloadEntity lastDownload = downloads.get(0);
            if (lastDownload.getCurrPageHash() != lastDownload.getPrevPageHash()) {
                offset = String.format(FILE_TEMPLATE, lastDownload.getPrevPageHash());
            }
        }
        return offset;
    }

    private static DownloadListEntity buildListEntity(String version, String offset, List<DownloadEntity> downloads) {
        DownloadListEntity listEntity = new DownloadListEntity();
        listEntity.setVersion(version);
        listEntity.setOffset(offset);
        listEntity.getDownload().addAll(downloads);
        return listEntity;
    }

}
