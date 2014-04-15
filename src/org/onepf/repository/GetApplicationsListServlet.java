package org.onepf.repository;

import org.onepf.repository.model.GetApplicationsRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.ResponseWriter;
import org.onepf.repository.api.responsewriter.XmlResponseWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 *
 * This Servlet returns list of all packages stored in repository.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class GetApplicationsListServlet extends BaseServlet {

    private final static String fileTemplate = "applist_%d.xml";
    private static final String PARAMETER_PAGE = "page";

    private GetApplicationsRequestHandler appLister;

    @Override
    public void init() throws ServletException {
        super.init();
        appLister = getRepositoryFactory().createApplicationsHandler();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {


        String page = request.getParameter(PARAMETER_PAGE);

        try {
            List<ApplicationDescriptor> apps = appLister.getApplications(page != null? Integer.valueOf(page) : -1);
            ResponseWriter responseWriter = new XmlResponseWriter();
            String prevOffset = null;
            String lastUpdated = null;
            if (apps.size() > 0 ) {
                ApplicationDescriptor lastApp = apps.get(0);
                lastUpdated = lastApp.lastUpdated;
                if (lastApp.currPageHash != lastApp.prevPageHash) {
                    String  url = request.getRequestURL().toString();
                    url = url.substring(0, url.lastIndexOf('/'));
                    prevOffset = url + '/' + String.format(fileTemplate, apps.get(0).prevPageHash);
                }

            }
            responseWriter.writeApplications(response.getWriter(), apps, prevOffset);
        } catch (WriteException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

}
