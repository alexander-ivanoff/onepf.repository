package org.onepf.repository;

import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.DefaultHttpClient;
import org.onepf.repository.api.ParserFactory;
import org.onepf.repository.appstorelooter.GetAppListRequest;
import org.onepf.repository.model.RepositoryConfigurator;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import java.io.File;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Created by ivanoff on 02.04.14.
 */
public class AppContextListener implements ServletContextListener {

    private ScheduledExecutorService scheduler;
    private ParserFactory parserFactory;

    private HttpClient httpClient;

    @Override
    public void contextInitialized(final ServletContextEvent servletContextEvent) {
        final ServletContext context = servletContextEvent.getServletContext();
        final RepositoryFactory factory = RepositoryConfigurator.getRepositoryFactory(context);
        AppstoreAuthenticator authenticator = RepositoryConfigurator.getAppstoreAuthenticator(context);

        scheduler = Executors.newSingleThreadScheduledExecutor();
        httpClient = new DefaultHttpClient();
        parserFactory = ParserFactory.getXmlParserFactory();
        Map<String, AppstoreDescriptor> appstores = null;
        try {
            appstores = authenticator.getAppstores();
        } catch (DataException e) {
            e.printStackTrace();
        }
        if (appstores != null) {
            for (AppstoreDescriptor appstore : appstores.values()) {
                if (appstore.appstoreId.equals("localstore")) //TEST PURPOSES ONLY
                    scheduler.scheduleAtFixedRate(new GetAppListRequest(parserFactory, factory, httpClient, appstore, new File(context.getRealPath("/uploads/"))), 30, 30, TimeUnit.SECONDS);
            }
        }

    }

    @Override
    public void contextDestroyed(ServletContextEvent servletContextEvent) {
        scheduler.shutdownNow();
        httpClient.getConnectionManager().shutdown();
    }
}