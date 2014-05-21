package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.protocol.HttpContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.model.FileType;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.UploadAppdfRequestHandler;
import org.onepf.repository.model.services.*;
import org.onepf.repository.model.services.Error;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.util.*;

/**
 * This class loads appdf files from remote appstore by their ApplicationDescriptors and store it
 * to repository.
 *
 * @author Alexander Ivanoff
 */
public class ApplicationsLoader {

    private final Logger alarmCauseLogger = LogManager.getLogger("AlarmCauseLogger");

    /**
     * This class describes request parameters
     */
    public static class Request {

        private AppstoreEntity appstore;
        private Set<ApplicationEntity> application;

        /**
         * @param appstore - AppstoreDescriptor to get appdf files from
         * @param applications - set of ApplicationDescriptor to get from remote appstore
         */
        public Request(AppstoreEntity appstore, Set<ApplicationEntity> applications) {
            this.appstore = appstore;
            this.application = applications;
        }
    }
    private File uploadDir;

    private HttpClient httpClient;
    private final HttpContext httpContext;

    private RepositoryFactory factory;

    private UploadAppdfRequestHandler appdfHandler;

    private Random random = new Random();

    public ApplicationsLoader(RepositoryFactory factory, HttpClient httpClient, HttpContext context, File uploadDir) {
        this.httpClient = httpClient;
        this.httpContext = context;
        this.factory = factory;
        appdfHandler = factory.createAppDFFileHandler();
        this.uploadDir = uploadDir;
        uploadDir.mkdirs();
    }

    /**
     * main method to load appdfs with options provided in request
     *
     * @param request
     * @throws IOException
     */
    public void loadApplications(final Request request) throws IOException {

        final AppstoreEntity appstore = request.appstore;
        final Set<ApplicationEntity> applications = request.application;

        Map<ApplicationEntity, String> failedAppsWithReason = loadApplicationsInt(appstore, applications);
        // trying one more time for failed packages:
        failedAppsWithReason = loadApplicationsInt(appstore, failedAppsWithReason.keySet());
        // if second try failed, log to alarm file
        if (failedAppsWithReason.size() > 0) {
            for (ApplicationEntity failedToLoadApp : failedAppsWithReason.keySet()) {
                alarmCauseLogger.error("failed to load package: {}, reason {}", failedToLoadApp.getPackageName(), failedAppsWithReason.get(failedToLoadApp));
            }
        }
    }

    /**
     * Internal method to load appdf files from appstore
     *
     * @param appstore - AppstoreDescriptor to get appdf files from
     * @param apps - set of ApplicationDescriptor to get from remote appstore
     * @return Map of ApplicationDescriptor and String represented reason why it was failed
     * @throws IOException
     */
    private Map<ApplicationEntity, String> loadApplicationsInt(final AppstoreEntity appstore, final Set<ApplicationEntity> apps) throws IOException {
        Map<ApplicationEntity, String> failedAppsWithReason = new HashMap<ApplicationEntity, String>();
        File tmpFile = null;
        String url;
        URI uri;
        for (ApplicationEntity appToLoad : apps) {
            try {
                boolean needUpdate = true;
                List<ApplicationEntity> appLog = factory.getDataService().getApplicationsLog(appToLoad.getPackageName(), -1);
                if (appLog.size() > 0) {
                    // check that uploading store is home store
                    if (!appLog.get(0).getAppstoreId().equals(appstore.getAppstoreId())) {
                        throw new DataException(Error.INTERNAL_ERROR.withMessage(
                                String.format("Store '%s' is not home store for package '%s'",
                                        appstore.getAppstoreId(), appToLoad.getPackageName())));
                    }
                    for (ApplicationEntity app : appLog) {
                        // check if there is appdf file with the same hash, if it is here means appdf is up to date
                        if (app.getAppdfHash().equals(appToLoad.getAppdfHash())) {
                            needUpdate = false;
                            break;
                        }
                    }
                }
                if (needUpdate) {
                    uri = RequesterUtils.buildRequestUri(ApiMapping.GET_APPDF.getMethodUrl(appstore.getOpenaepUrl()),
                            appstore.getAppstoreAccessToken(), appToLoad.getPackageName());
                    HttpGet httpGet = new HttpGet(uri);
                    httpGet.addHeader("authToken", appstore.getAppstoreAccessToken());
                    HttpResponse response = httpClient.execute(httpGet, httpContext);

                    int result = response.getStatusLine().getStatusCode();

                    if (result == HttpStatus.SC_OK) {
                        tmpFile = storeToUploadDir(response.getEntity().getContent(), appToLoad.getPackageName());
                        appdfHandler.processFile(tmpFile, appToLoad, appLog, appstore);
                    } else {
                        failedAppsWithReason.put(appToLoad, response.getStatusLine().toString());
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
                failedAppsWithReason.put(appToLoad, e.getMessage());
            } finally {
                if (tmpFile != null) {
                    tmpFile.delete();
                }
            }
        }
        return failedAppsWithReason;
    }

    /**
     * Store appdf file from input stream to temporary directory
     *
     * @param is
     * @param packageName
     * @return stored file object
     * @throws IOException
     */
    private File storeToUploadDir(InputStream is, String packageName) throws IOException {
        File uploadedFile = new File(uploadDir,  FileType.APPDF.addExtension(random.nextInt() + "_" + packageName));
        ReadableByteChannel rbc = Channels.newChannel(is);
        FileOutputStream fos = new FileOutputStream(uploadedFile);
        fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
        return uploadedFile;
    }


}
