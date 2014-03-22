package org.onepf.repository;

import org.onepf.repository.utils.responsewriter.WriteException;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.XmlResponseWriter;
import org.onepf.repository.model.ApplicationsList;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * Created by ivanoff on 11.03.14.
 */
public class GetApplicationsListServlet extends BaseServlet {

    private ApplicationsList appLister;

    @Override
    public void init() throws ServletException {
        super.init();
        appLister = getRepositoryFactory().createApplicationsList();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        List<ApplicationDescriptor> apps = appLister.getApplications(null);
        ResponseWriter responseWriter = new XmlResponseWriter();
        try {
            responseWriter.writeApplications(response.getWriter(), apps, 0);
        } catch (WriteException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
