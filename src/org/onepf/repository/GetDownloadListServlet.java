package org.onepf.repository;

import org.onepf.repository.model.GetDownloadsRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.WriteException;
import org.onepf.repository.utils.responsewriter.XmlResponseWriter;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * Created by ivanoff on 11.03.14.
 */
public class GetDownloadListServlet extends BaseServlet {

    private static final String PARAMETER_PACKAGE = "package";
    private static final String PARAMETER_PAGE = "page";

    private final static String fileTemplate = "downloads_%s_%d.xml";

    private GetDownloadsRequestHandler list;

    @Override
    public void init() throws ServletException {
        super.init();
        list = getRepositoryFactory().createDownloadsList();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        String packageName = request.getParameter(PARAMETER_PACKAGE);
        if (packageName == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "No package defined");
            return;
        }

        String page = request.getParameter(PARAMETER_PAGE);


        try {
            List<DownloadDescriptor> downloads = list.getDownloads(packageName, page != null? Integer.valueOf(page) : -1);
            ResponseWriter responseWriter = new XmlResponseWriter();
            String prevFileLink = null;
            String lastUpdated = null;
            if (downloads.size() > 0 ) {
                DownloadDescriptor lastDownload = downloads.get(0);
                lastUpdated = lastDownload.lastUpdate;
                if (lastDownload.currPageHash != lastDownload.prevPageHash) {
                    prevFileLink = String.format(fileTemplate, packageName, lastDownload.prevPageHash);
                }

            }
            responseWriter.writeDownloads(response.getWriter(), downloads, lastUpdated, prevFileLink);
        } catch (WriteException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
