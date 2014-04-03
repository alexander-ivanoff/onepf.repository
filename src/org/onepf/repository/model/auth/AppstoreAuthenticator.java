package org.onepf.repository.model.auth;

import org.onepf.repository.model.BaseRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * Created by ivanoff on 14.03.14.
 */
public class AppstoreAuthenticator extends BaseRequestHandler {

    private static final String AUTH_TOKEN = "authToken";

    private Map<String, AppstoreDescriptor> appstores;

    public AppstoreAuthenticator(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }


    public boolean isAuthorized(HttpServletRequest request) throws DataException {
        return getAuthorizedAppstore(request) != null;
    }

    public boolean isAuthorized(Map<String, String> parameters) throws DataException {
        return getAuthorizedAppstore(parameters) != null;
    }

    public AppstoreDescriptor getAuthorizedAppstore(HttpServletRequest request) throws DataException {
        String token = request.getHeader(AUTH_TOKEN);
        if (token == null) {
            token = request.getParameter(AUTH_TOKEN);
        }
        return getAuthorized(token);
    }

    public AppstoreDescriptor getAuthorizedAppstore(Map<String, String> parameters) throws DataException {
        return getAuthorized(parameters.get(AUTH_TOKEN));
    }

    private AppstoreDescriptor getAuthorized(String token) throws DataException {
        return token != null ? getAppstores().get(token) : null;
    }

    public Map<String, AppstoreDescriptor> getAppstores() throws DataException {
        if (appstores == null) {
            appstores = getAppstoresInt();
        }
        return appstores;
    }


    private Map<String, AppstoreDescriptor> getAppstoresInt() throws DataException {
        long time = System.currentTimeMillis();
        Map<String, AppstoreDescriptor> apps = dataService.getAppstores();
        System.out.println("List appstores time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return apps;
    }
}
