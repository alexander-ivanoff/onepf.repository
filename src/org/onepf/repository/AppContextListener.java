package org.onepf.repository;

import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.DefaultHttpClient;
import org.onepf.repository.appstorelooter.GetAppListRequest;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Created by ivanoff on 02.04.14.
 */
public class AppContextListener implements ServletContextListener {

    private ScheduledExecutorService scheduler;

    private HttpClient httpClient;

    @Override
    public void contextInitialized(ServletContextEvent servletContextEvent) {
        scheduler = Executors.newSingleThreadScheduledExecutor();
        httpClient = new DefaultHttpClient();
        scheduler.scheduleAtFixedRate(new GetAppListRequest(null, httpClient), 30, 30, TimeUnit.SECONDS);

    }

    @Override
    public void contextDestroyed(ServletContextEvent servletContextEvent) {
        scheduler.shutdownNow();
        httpClient.getConnectionManager().shutdown();
    }
}