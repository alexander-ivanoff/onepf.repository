package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.protocol.HttpContext;
import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.HashSet;
import java.util.Set;

/**
 * This class loads set of ApplicationDesription to update from remote appstore.
 *
 * @author Alexander Ivanoff
 */
public class ApplicationsToUpdateLoader {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    /**
     * This class describes request parameters
     */
    public static class Request {

        private AppstoreEntity appstore;
        private LastUpdateDescriptor prevUpdate;

        /**
         * @param appstore - AppstoreDescriptor to get appdf files from
         * @param prevUpdate - object with information about previous update or 'null' if there is first update.
         */
        public Request(AppstoreEntity appstore, LastUpdateDescriptor prevUpdate) {
            if (appstore == null) {
                throw new NullPointerException("appstore can't be null");
            }
            if (prevUpdate != null && !appstore.getAppstoreId().equals(prevUpdate.appstoreId)) {
                throw new IllegalArgumentException("prevUpdate from other appstore");
            }
            this.appstore = appstore;
            this.prevUpdate = prevUpdate;
        }
    }

    /**
     * This class describes response parameters
     *
     * <b>appsToUpdate</b> - set of ApplicationDescriptor to update
     * <b>lastUpdate</b> - object with information about this update
     *
     */
    public static class Response {

        private Set<ApplicationEntity> appsToUpdate;
        private LastUpdateDescriptor lastUpdate;

        public LastUpdateDescriptor getLastUpdate() {
            return lastUpdate;
        }

        public Set<ApplicationEntity> getAppsToUpdate() {
            return appsToUpdate;
        }
    }

    //private ParserFactory parserFactory;
    private HttpClient httpClient;
    private final HttpContext httpContext;


    public ApplicationsToUpdateLoader(/*final ParserFactory parserFactory,*/ final HttpClient httpClient, HttpContext context) {
        this.httpClient = httpClient;
        this.httpContext = context;
        //this.parserFactory = parserFactory;
    }

    /**
     * main method to load set of ApplicationDescriptor by options provided in request
     *
     * @param request
     * @return response object with set of ApplicationDescriptor and object with information about this update
     * @throws IOException
     */
    public Response getUpdates(final Request request) throws IOException{
        Response response = new Response();
        final AppstoreEntity appstore = request.appstore;
        final LastUpdateDescriptor prevUpdate = request.prevUpdate;

        Set<ApplicationEntity> appsToUpdate = new HashSet<ApplicationEntity>();
        String hash = null;
        LastUpdateDescriptor lastUpdate = null;

        String url = ApiMapping.LIST_APPLICATIONS.getMethodUrl(appstore.getOpenaepUrl());
        int iterations = 0;
        do {
            HttpGet httpGet = new HttpGet(url);
            httpGet.addHeader("authToken", appstore.getAppstoreAccessToken());

            HttpResponse httpResponse = httpClient.execute(httpGet, httpContext);

            int result = httpResponse.getStatusLine().getStatusCode();

            if (result == HttpStatus.SC_OK) {
                /*
                ListParser<ApplicationEntity, ApplicationListHeaderDescriptor> appParser = parserFactory.getApplicationParser(appsToUpdate);
                hash = parserFactory.parse(appParser, httpResponse.getEntity().getContent());
//                url = appParser.getHeader().offset;
                if (lastUpdate == null) {
                    lastUpdate = new LastUpdateDescriptor();
                    lastUpdate.appstoreId = appstore.appstoreId;
                    lastUpdate.lastResponseDatetime = dateFormat.format(new Date(System.currentTimeMillis()));
                    lastUpdate.lastResponseHash = hash;
//                    lastUpdate.prevOffset = appParser.getHeader().offset;
                }
                */
            } else {
                throw new IOException("Applist request failed with result: " + httpResponse.getStatusLine());
            }
            iterations++;
        } while (url != null && (prevUpdate == null || !url.equals(prevUpdate.prevOffset)));
        if (iterations == 1 && prevUpdate != null && prevUpdate.lastResponseHash.equals(hash)) {
            //do nothing - return empty response
        } else {
            response.lastUpdate = lastUpdate;
            response.appsToUpdate = appsToUpdate;
        }
        return response;
    }

}
