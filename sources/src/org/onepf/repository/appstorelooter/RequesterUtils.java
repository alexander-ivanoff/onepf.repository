package org.onepf.repository.appstorelooter;

import org.apache.http.client.utils.URIBuilder;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * Created by ivanoff on 25.04.14.
 */
public class RequesterUtils {

    public static URI buildRequestUri(String baseUrl, String authToken, String packageName) throws URISyntaxException {
        URIBuilder builder = null;
        builder = new URIBuilder(baseUrl);
        builder.addParameter("authToken", authToken);
        if (packageName != null) {
            builder.addParameter("package",packageName);
        }
        return builder.build();
    }
}
