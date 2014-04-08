package org.onepf.repository.appstorelooter;

import org.apache.http.client.HttpClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.ParserFactory;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.auth.AppstoreDescriptor;

import java.io.File;
import java.util.List;

/**
 * Created by ivanoff on 01.04.14.
 */
public class GetAppListRequest implements Runnable {

    private final Logger alarmCauseLogger = LogManager.getLogger("AlarmCauseLogger");
    private final Logger logger = LogManager.getLogger(GetAppListRequest.class.getName());

    private AppstoreDescriptor appstore;

    private HttpClient httpClient;
    private RepositoryFactory repositoryFactory;
    private ParserFactory parserFactory;
    private File uploadDir;

    private ApplicationsToUpdateLoader applicationsToUpdateLoader;

    public GetAppListRequest(
            ParserFactory parserFactory,
            RepositoryFactory repositoryFactory,
            HttpClient httpClient,
            AppstoreDescriptor appstore,
            File uploadDir) {
        this.appstore = appstore;
        this.httpClient = httpClient;
        this.repositoryFactory = repositoryFactory;
        this.parserFactory = parserFactory;
        this.uploadDir = uploadDir;
        applicationsToUpdateLoader = new ApplicationsToUpdateLoader(parserFactory, httpClient);
    }

    @Override
    public void run() {
        try {
            List<LastUpdateDescriptor> updates = repositoryFactory.getDataService().getLastUpdate(appstore.appstoreId);
            LastUpdateDescriptor lastUpdateDescriptor = (updates.size() > 0) ? updates.get(0) : null;
            ApplicationsToUpdateLoader.Response appsToUpdateResponse =
                    applicationsToUpdateLoader.getUpdates(new ApplicationsToUpdateLoader.Request(appstore, lastUpdateDescriptor));
            if (appsToUpdateResponse.getAppsToUpdate() == null) {
                logger.info("Update is not needed. Everything is up to date!");
            } else {
                logger.info("Updating: {}", appsToUpdateResponse.getLastUpdate().lastResponseHash);
                ApplicationsLoader appsLoader = new ApplicationsLoader(repositoryFactory, httpClient, uploadDir);
                appsLoader.loadApplications(new ApplicationsLoader.Request(appstore, appsToUpdateResponse.getAppsToUpdate()));
                // if everything was ok, store last Update
                repositoryFactory.getDataService().saveLastUpdate(appsToUpdateResponse.getLastUpdate());
            }
        } catch (Exception e) {
            alarmCauseLogger.error("Failed to get applications list from {}, reason {}", appstore.appstoreId, e.getMessage());
        }
    }

}
