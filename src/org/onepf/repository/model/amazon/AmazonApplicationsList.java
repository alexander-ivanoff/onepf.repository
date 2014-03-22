package org.onepf.repository.model.amazon;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import com.amazonaws.services.dynamodbv2.model.QueryResult;
import com.amazonaws.services.s3.model.S3ObjectSummary;
import org.onepf.repository.model.Options;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.model.ApplicationsList;
import org.onepf.repository.model.ObjectToDownload;
import org.onepf.repository.model.amazon.db.AmazonAppEntity;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 12.03.14.
 */
public class AmazonApplicationsList extends ApplicationsList {

    private AmazonDynamoDB amazonDynamoDB;

    private AmazonRepositoryFactory.RepositoryOptions repositoryOptions;
    private ObjectToDownload.ObjectOptions options;

    private List<S3ObjectSummary> summaries;

    public AmazonApplicationsList(AmazonServices amazonServices, AmazonRepositoryFactory.RepositoryOptions repositoryOptions) {
        this.amazonDynamoDB = amazonServices.getAmazonDynamoDB();
        this.repositoryOptions = repositoryOptions;
    }

    @Override
    public List<ApplicationDescriptor> getApplications(Options options) {

        long time = System.currentTimeMillis();

        QueryRequest queryRequest = AmazonAppEntity.searchRequestByLastUpdatedTime(0).withTableName(repositoryOptions.packageTable);

        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<ApplicationDescriptor> apps = new ArrayList<ApplicationDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            apps.add(AmazonAppEntity.getDescriptor(item));
        }
        System.out.println("List applications time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return apps;
    }
}
