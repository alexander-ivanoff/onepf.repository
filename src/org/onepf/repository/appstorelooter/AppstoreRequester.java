package org.onepf.repository.appstorelooter;

import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.PoolingClientConnectionManager;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.model.RepositoryConfigurator;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.services.DataException;

import javax.servlet.ServletContext;
import java.io.File;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

/**
 * This class schedule GetAppListRequest for all known appstores.
 *
 * @see GetAppListRequest
 * @author Alexander Ivanov
 */
public class AppstoreRequester {

    private static int  POLLING_PERIOD = 30; // polling period in seconds
    private static int CONNECTIONS_PER_STORE = 5;

    private HttpClient httpClient;
    private ScheduledExecutorService scheduler;

    //private ParserFactory parserFactory;
    private RepositoryFactory repositoryFactory;
    private AppstoreAuthenticator appstoreAuthenticator;

    private File uploadDir;

    public AppstoreRequester(ServletContext context) {
        repositoryFactory = RepositoryConfigurator.getRepositoryFactory(context);
        appstoreAuthenticator = RepositoryConfigurator.getAppstoreAuthenticator(context);
        //parserFactory = ParserFactory.getXmlParserFactory();

        uploadDir = new File(context.getRealPath("/uploads/"));
    }

    /**
     * Schedule GetAppListRequest
     */
    public void  start() {

        Map<String, AppstoreEntity> appstores = null;
        try {
            appstores = appstoreAuthenticator.getAppstores();
        } catch (DataException e) {
            e.printStackTrace();
        }
        if (appstores != null) {
            // creating multithreaded HttpClient
            PoolingClientConnectionManager cm = new PoolingClientConnectionManager();
            cm.setMaxTotal(appstores.size() * CONNECTIONS_PER_STORE);
            cm.setDefaultMaxPerRoute(CONNECTIONS_PER_STORE);
            httpClient = new DefaultHttpClient(cm);
            // schedule GetAppListRequests
            scheduler = Executors.newScheduledThreadPool(appstores.size());
            for (AppstoreEntity appstore : appstores.values()) {
                /*
                if (appstore.appstoreId.equals("onepf.repository")) { //TEST PURPOSES ONLY
                    cm.setDefaultMaxPerRoute(CONNECTIONS_PER_STORE);
                    scheduler.scheduleAtFixedRate(
                            new GetAppListRequest(parserFactory, repositoryFactory, httpClient, appstore, uploadDir ),
                            POLLING_PERIOD, POLLING_PERIOD, TimeUnit.SECONDS);
                }
                */
            }
        }
    }

    /**
     * shutdown all scheduled GetAppListRequest and httpClient
     */
    public void stop() {
        if (scheduler != null) { scheduler.shutdownNow(); }
        if (httpClient != null) { httpClient.getConnectionManager().shutdown(); }
    }
}
