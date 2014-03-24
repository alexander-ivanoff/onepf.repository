package org.onepf.repository.model.auth;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * Created by ivanoff on 14.03.14.
 *
 */
public abstract class AppstoreAuthenticator {

    public abstract boolean isAuthorized(HttpServletRequest request);
    public abstract boolean isAuthorized(Map<String, String> parameters);

    public abstract AppstoreDescriptor getAuthorizedAppstore(HttpServletRequest request);
    public abstract AppstoreDescriptor getAuthorizedAppstore(Map<String, String> parameters);
}
