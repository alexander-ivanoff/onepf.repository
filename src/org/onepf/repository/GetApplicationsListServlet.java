package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseWriterV2;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.responsewriter.entity.ApplicationListEntity;
import org.onepf.repository.api.responsewriter.entity.ObjectFactory;
import org.onepf.repository.api.xmlapi.XmlResponseWriterV2;
import org.onepf.repository.model.GetApplicationsRequestHandler;
import org.onepf.repository.model.services.DataException;

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

    private static final ResponseWriterV2 responseWriter = initResponseWriter();

    private static ResponseWriterV2 initResponseWriter() {
        XmlResponseWriterV2 responseWriter = null;
        try {
            responseWriter = new XmlResponseWriterV2(ObjectFactory._ApplicationList_QNAME, ApplicationListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return responseWriter;
    }

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
            List<ApplicationEntity> apps = appLister.getApplications(page != null? Integer.valueOf(page) : -1);
            String offset = getOffset(request, apps);
            responseWriter.write(response.getOutputStream(), buildListEntity("1", offset, apps));
        } catch (WriteException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    private static String getOffset(HttpServletRequest request, List<ApplicationEntity> apps) {
        String offset = null;
        if (apps.size() > 0 ) {
            ApplicationEntity lastApp = apps.get(0);
            if (lastApp.getCurrPageHash() != lastApp.getPrevPageHash()) {
                String  url = request.getRequestURL().toString();
                url = url.substring(0, url.lastIndexOf('/'));
                offset = url + '/' + String.format(fileTemplate, apps.get(0).getPrevPageHash());
            }
        }
        return offset;
    }

    private static ApplicationListEntity buildListEntity(String version, String offset, List<ApplicationEntity> apps) {
        ApplicationListEntity appListEntity = new ApplicationListEntity();
        appListEntity.setVersion(version);
        appListEntity.setOffset(offset);
        appListEntity.getApplication().addAll(apps);
        return  appListEntity;
    }

}
