package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.model.FileType;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.UploadAppdfRequestHandler;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.StorageException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Created by ivanoff on 03.04.14.
 */
public class ApplicationsLoader {

    private final Logger alarmCauseLogger = LogManager.getLogger("AlarmCauseLogger");

    public static class Request {

        private AppstoreDescriptor appstore;
        private Set<ApplicationDescriptor> application;

        public Request(AppstoreDescriptor appstore, Set<ApplicationDescriptor> applications) {
            this.appstore = appstore;
            this.application = applications;
        }
    }
    private File uploadDir;

    private HttpClient httpClient;
    private RepositoryFactory factory;

    private UploadAppdfRequestHandler appdfHandler;

    private Random random = new Random();

    public ApplicationsLoader(RepositoryFactory factory, HttpClient httpClient, File uploadDir) {
        this.httpClient = httpClient;
        this.factory = factory;
        appdfHandler = factory.createAppDFFileHandler();
        this.uploadDir = uploadDir;
        uploadDir.mkdirs();
    }

    public void loadApplications(final Request request) throws IOException {

        final AppstoreDescriptor appstore = request.appstore;
        final Set<ApplicationDescriptor> applications = request.application;

        Map<ApplicationDescriptor, String> failedAppsWithReason = loadApplications(appstore, applications);
        // trying one more time:
        failedAppsWithReason = loadApplications(appstore, failedAppsWithReason.keySet());
        // if second try failed, log to alarm file
        if (failedAppsWithReason.size() > 0) {
            for (ApplicationDescriptor failedToLoadApp : failedAppsWithReason.keySet()) {
                alarmCauseLogger.error("failed to load package: {}, reason {}", failedToLoadApp.packageName, failedAppsWithReason.get(failedToLoadApp));
            }
        }
    }

    /**
     *
     * @param appstore
     * @param apps
     * @return Map of ApplicationDescriptor and String represented reason why it was failed
     * @throws IOException
     */
    private Map<ApplicationDescriptor, String> loadApplications(final AppstoreDescriptor appstore, final Set<ApplicationDescriptor> apps) throws IOException {
        Map<ApplicationDescriptor, String> failedAppsWithReason = new HashMap<ApplicationDescriptor, String>();
        String url;
        for (ApplicationDescriptor appToLoad : apps) {
            try {
                // check if there are appdf file with the same hash, it is here means appdf is up to date
                List<ApplicationDescriptor> uptodateApp = factory.getDataService().getApplicationByHash(appToLoad.packageName, appToLoad.appdfHash);
                if (uptodateApp.size() == 0) {
                    url = ApiMapping.GET_APPDF.getMethodUrl(appstore.openaepUrl) + "?package=" + appToLoad.packageName;
                    HttpGet httpGet = new HttpGet(url);
                    httpGet.addHeader("authToken", appstore.appstoreAccessToken);
                    HttpResponse response = httpClient.execute(httpGet);

                    int result = response.getStatusLine().getStatusCode();

                    if (result == HttpStatus.SC_OK) {
                        File  file = storeToUploadDir(response.getEntity().getContent(), appToLoad.packageName);
                        appdfHandler.processFile(file, "No contact", appstore);
                    } else {
                        failedAppsWithReason.put(appToLoad, response.getStatusLine().toString());
                    }
                }
            } catch (StorageException e) {
                e.printStackTrace();
                failedAppsWithReason.put(appToLoad, e.getMessage());
            } catch (DataException e) {
                e.printStackTrace();
                failedAppsWithReason.put(appToLoad, e.getMessage());
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
                failedAppsWithReason.put(appToLoad, e.getMessage());
            }
        }
        return failedAppsWithReason;
    }

    private File storeToUploadDir(InputStream is, String packageName) throws IOException {
        File uploadedFile = new File(uploadDir,  FileType.APPDF.addExtention(random.nextInt() + "_" + packageName));
        ReadableByteChannel rbc = Channels.newChannel(is);
        FileOutputStream fos = new FileOutputStream(uploadedFile);
        fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
        return uploadedFile;
    }


}
