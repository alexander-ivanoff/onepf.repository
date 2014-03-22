package org.onepf.repository.model.auth;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Map;
import java.util.Properties;

/**
 * Created by ivanoff on 14.03.14.
 */
public class SimpleAppstoreAuthenticator extends AppstoreAuthenticator {

    private static final String AUTH_TOKEN = "authToken";

    private Properties authProps = new Properties();

    public SimpleAppstoreAuthenticator(File authFile) {
        Reader reader = null;
        try {
            reader = new FileReader(authFile);
            authProps.load(reader);
        } catch (IOException e) {
            authProps = null;
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }
        }

    }

    @Override
    public boolean isAuthorized(HttpServletRequest request) {
        String token = request.getHeader(AUTH_TOKEN);
        if (token == null) {
            token = request.getParameter(AUTH_TOKEN);
        }
        return isAuthorized(token);
    }

    @Override
    public boolean isAuthorized(Map<String, String> parameters) {
        return isAuthorized(parameters.get(AUTH_TOKEN));
    }

    private boolean isAuthorized(String token) {
        return token != null ? authProps.containsValue(token) : false;
    }
}
