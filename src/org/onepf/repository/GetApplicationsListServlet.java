package org.onepf.repository;

import org.onepf.repository.model.GetApplicationsRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.utils.responsewriter.WriteException;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.XmlResponseWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * Created by ivanoff on 11.03.14.
 */
public class GetApplicationsListServlet extends BaseServlet {

    private GetApplicationsRequestHandler appLister;

    @Override
    public void init() throws ServletException {
        super.init();
        appLister = getRepositoryFactory().createApplicationsList();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        try {
            List<ApplicationDescriptor> apps = appLister.getApplications();
            ResponseWriter responseWriter = new XmlResponseWriter();
            responseWriter.writeApplications(response.getWriter(), apps, 0);
        } catch (WriteException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
