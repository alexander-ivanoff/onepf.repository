package org.onepf.repository.model.amazon;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import com.amazonaws.services.dynamodbv2.model.QueryResult;
import org.onepf.repository.model.amazon.db.AmazonAppstoreEntity;
import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.auth.AppstoreDescriptor;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

/**
 * Created by ivanoff on 14.03.14.
 */
public class AmazonAppstoreAuthenticator extends AppstoreAuthenticator {

    private static final String AUTH_TOKEN = "authToken";

    private AmazonDynamoDB amazonDynamoDB;
    private AmazonRepositoryFactory.RepositoryOptions repositoryOptions;

    Map<String, AppstoreDescriptor> appstores;

    public AmazonAppstoreAuthenticator(AmazonServices amazonServices, AmazonRepositoryFactory.RepositoryOptions repositoryOptions) {
        this.amazonDynamoDB = amazonServices.getAmazonDynamoDB();
        this.repositoryOptions = repositoryOptions;
    }

    @Override
    public boolean isAuthorized(HttpServletRequest request) {
        return getAuthorizedAppstore(request) != null;
    }

    @Override
    public boolean isAuthorized(Map<String, String> parameters) {
        return getAuthorizedAppstore(parameters) != null;
    }

    @Override
    public AppstoreDescriptor getAuthorizedAppstore(HttpServletRequest request) {
        String token = request.getHeader(AUTH_TOKEN);
        if (token == null) {
            token = request.getParameter(AUTH_TOKEN);
        }
        return getAuthorized(token);
    }

    @Override
    public AppstoreDescriptor getAuthorizedAppstore(Map<String, String> parameters) {
        return getAuthorized(parameters.get(AUTH_TOKEN));
    }

    private AppstoreDescriptor getAuthorized(String token) {
        if (appstores == null) {
            appstores = getAppstores();
        }
        return token != null ? appstores.get(token) : null;
    }


    private Map<String, AppstoreDescriptor> getAppstores() {

        long time = System.currentTimeMillis();

        QueryRequest queryRequest = AmazonAppstoreEntity.searchRequestAllAppstores().withTableName(repositoryOptions.appstoreTable);

        QueryResult result = amazonDynamoDB.query(queryRequest);

        Map<String, AppstoreDescriptor> apps = new HashMap<String, AppstoreDescriptor>();
        AppstoreDescriptor appstore = null;
        for (Map<String, AttributeValue> item : result.getItems()) {
            appstore = AmazonAppstoreEntity.getDescriptor(item);
            apps.put(appstore.authToken, appstore);
        }
        System.out.println("List appstores time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return apps;
    }
}
