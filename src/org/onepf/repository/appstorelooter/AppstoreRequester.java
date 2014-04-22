package org.onepf.repository.appstorelooter;

import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.PoolingClientConnectionManager;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.ApplicationListEntity;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.api.responsewriter.entity.ObjectFactory;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;
import org.onepf.repository.model.RepositoryConfigurator;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.services.DataException;

import javax.servlet.ServletContext;
import java.io.File;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * This class schedule GetAppListRequest and GetStatisticsRequest for all known appstores.
 *
 * @see GetAppListRequest
 * @see GetStatisticsRequest
 * @author Alexander Ivanov
 */
public class AppstoreRequester {

    private static int  POLLING_PERIOD = 30; // polling period in seconds
    private static int CONNECTIONS_PER_STORE = 5;

    private HttpClient httpClient;
    private ScheduledExecutorService scheduler;

    XmlResponseReaderWriter<ApplicationListEntity> xmlResponseWriterV2;
    private RepositoryFactory repositoryFactory;
    private AppstoreAuthenticator appstoreAuthenticator;

    private File uploadDir;
    private File tmpDir;

    public AppstoreRequester(ServletContext context) {
        repositoryFactory = RepositoryConfigurator.getRepositoryFactory(context);
        appstoreAuthenticator = RepositoryConfigurator.getAppstoreAuthenticator(context);
        try {
            xmlResponseWriterV2 = new XmlResponseReaderWriter<ApplicationListEntity>(ObjectFactory._ApplicationList_QNAME, ApplicationListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }

        uploadDir = new File(context.getRealPath("/uploads/"));
        tmpDir = new File(context.getRealPath("/tmp/"));
    }

    /**
     * Schedule GetAppListRequest and GetStatisticsRequest
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

            scheduler = Executors.newScheduledThreadPool(appstores.size());
            for (AppstoreEntity appstore : appstores.values()) {
                if (appstore.getAppstoreId().equals("onepf.repository")) { //TEST PURPOSES ONLY
                    cm.setDefaultMaxPerRoute(CONNECTIONS_PER_STORE);
                    // schedule GetAppListRequests
                    scheduler.scheduleAtFixedRate(
                            new GetAppListRequest(xmlResponseWriterV2, repositoryFactory, httpClient, appstore, uploadDir),
                            POLLING_PERIOD, POLLING_PERIOD, TimeUnit.SECONDS);
                    // schedule GetStatisticsRequests
                    scheduler.scheduleAtFixedRate(
                            new GetStatisticsRequest(xmlResponseWriterV2, repositoryFactory, httpClient, appstore, tmpDir),
                            POLLING_PERIOD, POLLING_PERIOD, TimeUnit.SECONDS);
                }
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
