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
                    prevOffset = String.format(fileTemplate, apps.get(0).prevPageHash);
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
