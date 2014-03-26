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
    private static final String PARAMETER_DATE = "date";

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

        // Date is optional parameter. Can be null
        String date = request.getParameter(PARAMETER_DATE);
        if (date != null ) {
            // TODO PARSE DATE
        }

        try {
            List<DownloadDescriptor> downloads = list.getDownloads(packageName, 0);
            ResponseWriter responseWriter = new XmlResponseWriter();
            responseWriter.writeDownloads(response.getWriter(), downloads);
        } catch (WriteException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
