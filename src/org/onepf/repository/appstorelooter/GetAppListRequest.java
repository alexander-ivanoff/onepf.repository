package org.onepf.repository.appstorelooter;

import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.onepf.repository.api.ParserFactory;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;

import java.io.IOException;
import java.util.List;

/**
 * Created by ivanoff on 01.04.14.
 */
public class GetAppListRequest implements Runnable {

    private AppstoreDescriptor appstore;

    private HttpClient httpClient;
    private RepositoryFactory repositoryFactory;
    private ParserFactory parserFactory;

    private ApplicationsToUpdateLoader applicationsToUpdateLoader;

    public GetAppListRequest(ParserFactory parserFactory, RepositoryFactory repositoryFactory, HttpClient httpClient, AppstoreDescriptor appstore) {
        this.appstore = appstore;
        this.httpClient = httpClient;
        this.repositoryFactory = repositoryFactory;
        this.parserFactory = parserFactory;

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
                System.out.print("update is not needed!");
            } else {
                System.out.print("updated: " + appsToUpdateResponse.getLastUpdate().lastResponseHash);
                for (ApplicationDescriptor app : appsToUpdateResponse.getAppsToUpdate()) {
                    System.out.println(app.packageName + ", " + app.lastUpdated);
                }
            }



        } catch (ClientProtocolException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (DataException e) {
            e.printStackTrace();
        } catch (ParserFactory.ParseException e) {
            e.printStackTrace();
        }


    }

}
