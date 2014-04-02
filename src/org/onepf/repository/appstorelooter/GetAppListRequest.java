package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.utils.Pair;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.xmlapi.ApiParser;
import org.onepf.repository.xmlapi.BaseListHeaderDescriptor;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by ivanoff on 01.04.14.
 */
public class GetAppListRequest implements Runnable {

    private AppstoreDescriptor appstore;
    private HttpClient httpClient;
    private DataService dataService;


    public GetAppListRequest(DataService dataService, HttpClient httpClient, AppstoreDescriptor appstore) {
        this.appstore = appstore;
        this.httpClient = httpClient;
        this.dataService = dataService;
    }

    @Override
    public void run() {
        try {
            List<LastUpdateInfo> updates = dataService.getLastUpdate(appstore.appstoreId);
            LastUpdateInfo lastUpdateInfo = new LastUpdateInfo(); // TODO load Last Update Info

            // String url = ApiMapping.LIST_APPLICATIONS.getMethodUrl(appstore.openaepUrl);
            String url = "http://localhost:8181/openaep/applist";

            HttpGet httpGet = new HttpGet(url);
            httpGet.addHeader("authToken", "TESTTESTTEST"); //TODO here should be appstore provided token

            Set<ApplicationDescriptor> apps = new HashSet<ApplicationDescriptor>();
            Pair<BaseListHeaderDescriptor, String> headerAndHash = null;

            HttpResponse response = httpClient.execute(httpGet);

            int result = response.getStatusLine().getStatusCode();

            if (result == HttpStatus.SC_OK) {
                headerAndHash = ApiParser.getApplications(apps, response.getEntity().getContent());
            }

            for (ApplicationDescriptor app : apps) {
                System.out.println("app: " + app.packageName + ", lastUpdated: " + app.lastUpdated);
            }

            // todo parse response and store it in map
        } catch (ApiParser.ParseException e) {
            e.printStackTrace();
        } catch (ClientProtocolException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (DataException e) {
            e.printStackTrace();
        }


    }

}
