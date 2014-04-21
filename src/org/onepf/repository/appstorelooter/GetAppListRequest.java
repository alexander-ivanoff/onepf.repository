package org.onepf.repository.appstorelooter;

import org.apache.http.client.HttpClient;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;
import org.onepf.repository.model.RepositoryFactory;

import java.io.File;
import java.util.List;

/**
 * This class updates appdf files from single remote appstore and store it to the repostitory.
 *
 * @author Alexander Ivanoff on 01.04.14.
 */
public class GetAppListRequest implements Runnable {

    private final Logger alarmCauseLogger = LogManager.getLogger("AlarmCauseLogger");
    private final Logger logger = LogManager.getLogger(GetAppListRequest.class.getName());

    private AppstoreEntity appstore;

    private HttpClient httpClient;

    private RepositoryFactory repositoryFactory;
    private XmlResponseReaderWriter xmlResponseWriter;
    private File uploadDir;

    private ApplicationsToUpdateLoader applicationsToUpdateLoader;
    private ApplicationsLoader appsLoader;

    public GetAppListRequest(
            XmlResponseReaderWriter xmlResponseWriter,
            RepositoryFactory repositoryFactory,
            HttpClient httpClient,
            AppstoreEntity appstore,
            File uploadDir) {
        this.appstore = appstore;
        this.httpClient = httpClient;
        this.repositoryFactory = repositoryFactory;
        this.xmlResponseWriter = xmlResponseWriter;
        this.uploadDir = uploadDir;
        HttpContext httpContext = new BasicHttpContext();
        applicationsToUpdateLoader = new ApplicationsToUpdateLoader(xmlResponseWriter, httpClient, httpContext);
        appsLoader = new ApplicationsLoader(repositoryFactory, httpClient, httpContext, uploadDir);
    }

    @Override
    public void run() {
        try {
            LastUpdateEntity lastUpdateDescriptor = repositoryFactory.getDataService().getLastUpdate(appstore.getAppstoreId());
            ApplicationsToUpdateLoader.Response appsToUpdateResponse =
                    applicationsToUpdateLoader.getUpdates(new ApplicationsToUpdateLoader.Request(appstore, lastUpdateDescriptor));
            if (appsToUpdateResponse.getAppsToUpdate() == null) {
                logger.info("Update is not needed. Everything is up to date!");
            } else {
                logger.info("Updating: {}", appsToUpdateResponse.getLastUpdate().getLastResponseHash());
                appsLoader.loadApplications(new ApplicationsLoader.Request(appstore, appsToUpdateResponse.getAppsToUpdate()));
                // if everything was ok, store last Update
                repositoryFactory.getDataService().saveLastUpdate(appsToUpdateResponse.getLastUpdate());
            }
        } catch (Exception e) {
            alarmCauseLogger.error("Failed to get applications list from {}, reason {}", appstore.getAppstoreId(), e.getMessage());
        }
    }

}
