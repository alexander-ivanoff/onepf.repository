package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
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
import java.util.List;
import java.util.Random;
import java.util.Set;

/**
 * Created by ivanoff on 03.04.14.
 */
public class ApplicationsLoader {

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

        String url;
        for (ApplicationDescriptor appToLoad : applications) {
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
                        System.out.println("APPLIST RESPONSE: " + result);
                    }
                }
            } catch (StorageException e) {
                e.printStackTrace();
            } catch (DataException e) {
                e.printStackTrace();
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            }
        }
    }

    private File storeToUploadDir(InputStream is, String packageName) throws IOException {
        File uploadedFile = new File(uploadDir,  FileType.APPDF.addExtention(random.nextInt() + "_" + packageName));
        ReadableByteChannel rbc = Channels.newChannel(is);
        FileOutputStream fos = new FileOutputStream(uploadedFile);
        fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
        return uploadedFile;
    }


}
