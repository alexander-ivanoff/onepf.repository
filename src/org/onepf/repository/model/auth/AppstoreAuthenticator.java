package org.onepf.repository.model.auth;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.model.BaseRequestHandler;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * This class used to manage list of appstores working with, and check incoming request for autorization.
 *
* @author Alexander Ivanoff on 14.03.14.
 */
public class AppstoreAuthenticator extends BaseRequestHandler {

    private static final String AUTH_TOKEN = "authToken";

    private final Logger logger = LogManager.getLogger(AppstoreAuthenticator.class.getName());

    private Map<String, AppstoreEntity> appstores;

    public AppstoreAuthenticator(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }


    /**
     *
     * check that HTTTP request contains authToken for known Appstore
     *
     * @param request incoming http request
     * @return true, if the appstore with given authToken is known
     * @throws DataException
     */
    public boolean isAuthorized(HttpServletRequest request) throws DataException {
        return getAuthorizedAppstore(request) != null;
    }

    /**
     *
     * check that incoming parameters contains authToken for known Appstore
     *
     * @param parameters Map of <key, value> parameters to test.
     * @return true, if parameters contains key="authToken" and known value of authToken
     * @throws DataException
     */
    public boolean isAuthorized(Map<String, String> parameters) throws DataException {
        return getAuthorizedAppstore(parameters) != null;
    }

    /**
     *
     * @param request HTTTP request contains authToken for known Appstore
     * @return AppstoreDescriptor, if requests contains authToken for known Appstore, null otherwise
     * @throws DataException
     */
    public AppstoreEntity getAuthorizedAppstore(HttpServletRequest request) throws DataException {
        String token = request.getHeader(AUTH_TOKEN);
        if (token == null) {
            token = request.getParameter(AUTH_TOKEN);
        }
        return getAuthorized(token);
    }

    /**
     *
     * @param parameters Map of <key, value> parameters to test.
     * @return AppstoreDescriptor, if parameters contains key="authToken" and vakue for known Appstore, null otherwise
     * @throws DataException
     */
    public AppstoreEntity getAuthorizedAppstore(Map<String, String> parameters) throws DataException {
        return getAuthorized(parameters.get(AUTH_TOKEN));
    }

    private AppstoreEntity getAuthorized(String token) throws DataException {
        return token != null ? getAppstores().get(token) : null;
    }

    /**
     *
     * @return list of all known appstores
     * @throws DataException
     */
    public Map<String, AppstoreEntity> getAppstores() throws DataException {
        if (appstores == null) {
            appstores = getAppstoresInt();
        }
        return appstores;
    }


    private Map<String, AppstoreEntity> getAppstoresInt() throws DataException {
        long time = System.currentTimeMillis();
        Map<String, AppstoreEntity> apps = dataService.getAppstores();
        logger.debug("List appstores time: {}", (System.currentTimeMillis() - time));
        return apps;
    }
}
