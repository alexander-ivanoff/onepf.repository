package org.onepf.repository;

import org.onepf.repository.appstorelooter.AppstoreRequester;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

/**
 * This class launch appstore requester before the web application start
 * and stop it after application destroy.
 *
 * @see AppstoreRequester
 * @author Alexander Ivanov
 */
public class AppContextListener implements ServletContextListener {

    private static final String PARAM_POLLSTORES = "pollStores";

    private static final String PARAM_VALUE_TRUE = "true";

    private AppstoreRequester appstoreRequester;

    @Override
    public void contextInitialized(final ServletContextEvent servletContextEvent) {
        final ServletContext context = servletContextEvent.getServletContext();

        appstoreRequester = new AppstoreRequester(context);

        if (PARAM_VALUE_TRUE.equals(context.getInitParameter(PARAM_POLLSTORES))) {
            appstoreRequester.start();
        }

    }

    @Override
    public void contextDestroyed(ServletContextEvent servletContextEvent) {
        appstoreRequester.stop();
    }
}