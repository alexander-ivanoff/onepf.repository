package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.ListParser;
import org.onepf.repository.api.ParserFactory;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationListHeaderDescriptor;
import org.onepf.repository.model.auth.AppstoreDescriptor;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by ivanoff on 03.04.14.
 */
public class ApplicationsToUpdateLoader {

    public static class Request {

        private AppstoreDescriptor appstore;
        private LastUpdateDescriptor prevUpdate;

        public Request(AppstoreDescriptor appstore, LastUpdateDescriptor prevUpdate) {
            if (appstore == null) {
                throw new NullPointerException("appstore can't be null");
            }
            if (prevUpdate != null && !appstore.appstoreId.equals(prevUpdate.appstoreId)) {
                throw new IllegalArgumentException("prevUpdate from other appstore");
            }
            this.appstore = appstore;
            this.prevUpdate = prevUpdate;
        }
    }

    public static class Response {

        private Set<ApplicationDescriptor> appsToUpdate;
        private LastUpdateDescriptor lastUpdate;

        public LastUpdateDescriptor getLastUpdate() {
            return lastUpdate;
        }

        public Set<ApplicationDescriptor> getAppsToUpdate() {
            return appsToUpdate;
        }
    }

    private ParserFactory parserFactory;
    private HttpClient httpClient;



    public ApplicationsToUpdateLoader(final ParserFactory parserFactory, final HttpClient httpClient) {
        this.httpClient = httpClient;
        this.parserFactory = parserFactory;
    }

    public Response getUpdates(final Request request) throws IOException, ParserFactory.ParseException {
        Response response = new Response();

        final AppstoreDescriptor appstore = request.appstore;
        final LastUpdateDescriptor prevUpdate = request.prevUpdate;

        Set<ApplicationDescriptor> appsToUpdate = new HashSet<ApplicationDescriptor>();
        String hash = null;
        LastUpdateDescriptor lastUpdate = null;

        String url = ApiMapping.LIST_APPLICATIONS.getMethodUrl(appstore.openaepUrl);
        int iterations = 0;
        do {
            HttpGet httpGet = new HttpGet(url);
            httpGet.addHeader("authToken", appstore.appstoreAccessToken);

            HttpResponse httpResponse = httpClient.execute(httpGet);

            int result = httpResponse.getStatusLine().getStatusCode();

            if (result == HttpStatus.SC_OK) {
                ListParser<ApplicationDescriptor, ApplicationListHeaderDescriptor> appParser = parserFactory.getApplicationParser(appsToUpdate);
                hash = parserFactory.parse(appParser, httpResponse.getEntity().getContent());
                url = appParser.getHeader().offset;
                if (lastUpdate == null) {
                    lastUpdate = new LastUpdateDescriptor();
                    lastUpdate.appstoreId = appstore.appstoreId;
                    lastUpdate.lastResponseHash = hash;
                    lastUpdate.prevOffset = appParser.getHeader().offset;
                }

            } else {
                System.out.println("APPLIST RESPONSE: " + result);
            }
            iterations++;
        } while (url != null && (prevUpdate == null || url != prevUpdate.prevOffset));
        if (iterations == 1 && prevUpdate != null && prevUpdate.lastResponseHash == hash) {
            //do nothind - return empty response
        } else {
            response.lastUpdate = lastUpdate;
            response.appsToUpdate = appsToUpdate;
        }
        return response;
    }

}
