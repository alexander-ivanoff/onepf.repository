package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.protocol.HttpContext;
import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.responsewriter.entity.ApplicationListEntity;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import java.io.IOException;
import java.net.URI;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * This class loads set of ApplicationDescription to update from remote appstore.
 *
 * @author Alexander Ivanoff
 */
public class ApplicationsToUpdateLoader {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    private static HexBinaryAdapter marshaler = new HexBinaryAdapter();

    /**
     * This class describes request parameters
     */
    public static class Request {

        private AppstoreEntity appstore;
        private LastUpdateEntity prevUpdate;

        /**
         * @param appstore - AppstoreDescriptor to get appdf files from
         * @param prevUpdate - object with information about previous update or 'null' if there is first update.
         */
        public Request(AppstoreEntity appstore, LastUpdateEntity prevUpdate) {
            if (appstore == null) {
                throw new NullPointerException("appstore can't be null");
            }
            if (prevUpdate != null && !appstore.getAppstoreId().equals(prevUpdate.getAppstoreId())) {
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
        private LastUpdateEntity lastUpdate;

        public LastUpdateEntity getLastUpdate() {
            return lastUpdate;
        }

        public Set<ApplicationEntity> getAppsToUpdate() {
            return appsToUpdate;
        }
    }

    private XmlResponseReaderWriter<ApplicationListEntity> xmlResponseWriter;
    private HttpClient httpClient;
    private final HttpContext httpContext;


    public ApplicationsToUpdateLoader(XmlResponseReaderWriter xmlResponseWriter, final HttpClient httpClient, HttpContext context) {
        this.httpClient = httpClient;
        this.httpContext = context;
        this.xmlResponseWriter = xmlResponseWriter;
    }

    /**
     * main method to load set of ApplicationDescriptor by options provided in request
     *
     * @param request
     * @return response object with set of ApplicationDescriptor and object with information about this update
     * @throws IOException
     */
    public Response getUpdates(final Request request) throws Exception {
        Response response = new Response();
        final AppstoreEntity appstore = request.appstore;
        final LastUpdateEntity prevUpdate = request.prevUpdate;

        Set<ApplicationEntity> appsToUpdate = new HashSet<ApplicationEntity>();
        String hash = null;
        LastUpdateEntity lastUpdate = null;

        String url = ApiMapping.LIST_APPLICATIONS.getMethodUrl(appstore.getOpenaepUrl());
        int iterations = 0;
        URI uri = null;
        do {
            uri = RequesterUtils.buildRequestUri(url, appstore.getAppstoreAccessToken(), null);
            HttpGet httpGet = new HttpGet(uri);
            httpGet.addHeader("authToken", appstore.getAppstoreAccessToken());

            HttpResponse httpResponse = httpClient.execute(httpGet, httpContext);

            int result = httpResponse.getStatusLine().getStatusCode();

            if (result == HttpStatus.SC_OK) {
                hash = null;
                MessageDigest md = MessageDigest.getInstance("MD5");
                DigestInputStream dis = new DigestInputStream(httpResponse.getEntity().getContent(), md);
                ApplicationListEntity applicationListEntity = xmlResponseWriter.read(ApplicationListEntity.class, dis);
                hash = marshaler.marshal(dis.getMessageDigest().digest());
                url = applicationListEntity.getOffset();
                appsToUpdate.addAll(applicationListEntity.getApplication());
                if (lastUpdate == null) {
                    lastUpdate = new LastUpdateEntity();
                    lastUpdate.setAppstoreId(appstore.getAppstoreId());
                    lastUpdate.setLastResponseDatetime(dateFormat.format(new Date(System.currentTimeMillis())));
                    lastUpdate.setLastResponseHash(hash);
                    lastUpdate.setPrevOffset(applicationListEntity.getOffset());
                }
            } else {
                throw new IOException("Applist request failed with result: " + httpResponse.getStatusLine());
            }
            iterations++;
        } while (url != null && (prevUpdate == null || !url.equals(prevUpdate.getPrevOffset())));
        if (iterations == 1 && prevUpdate != null && prevUpdate.getLastResponseHash().equals(hash)) {
            //do nothing - return empty response
        } else {
            response.lastUpdate = lastUpdate;
            response.appsToUpdate = appsToUpdate;
        }
        return response;
    }

}
