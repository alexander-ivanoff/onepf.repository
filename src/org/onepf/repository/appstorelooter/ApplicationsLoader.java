package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.model.RepositoryConfigurator;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.auth.AppstoreDescriptor;

import javax.servlet.ServletContext;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
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

    public static class Response {

    }

    File uploadDir;

    RepositoryFactory factory;
    private HttpClient httpClient;

    public ApplicationsLoader(ServletContext context, HttpClient httpClient) {
        this.httpClient = httpClient;
        this.factory = RepositoryConfigurator.getRepositoryFactory(context);
        new File(context.getRealPath("/uploads/"));
    }

    public void loadApplications(final Request request) throws IOException {

        final AppstoreDescriptor appstore = request.appstore;
        final

        boolean needUpdate = false;

        String url = ApiMapping.GET_APPDF.getMethodUrl(appstore.openaepUrl);
        int iterations = 0;
        do {
            HttpGet httpGet = new HttpGet(url);
            httpGet.addHeader("authToken", appstore.appstoreAccessToken);

            HttpResponse response = httpClient.execute(httpGet);

            int result = response.getStatusLine().getStatusCode();

            if (result == HttpStatus.SC_OK) {
                ReadableByteChannel rbc = Channels.newChannel(response.getEntity().getContent());
                FileOutputStream fos = new FileOutputStream("information.html");
                fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);

            } else {
                System.out.println("APPLIST RESPONSE: " + result);
            }
            iterations++;
        } while (true);
    }


}
