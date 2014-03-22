package org.onepf.repository.model.amazon;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import com.amazonaws.services.dynamodbv2.model.QueryResult;
import org.onepf.repository.model.DownloadsList;
import org.onepf.repository.model.Options;
import org.onepf.repository.model.amazon.db.AmazonDownloadEntity;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 12.03.14.
 */
public class AmazonDownloadsList extends DownloadsList {

    private AmazonDynamoDB amazonDynamoDB;

    private AmazonRepositoryFactory.RepositoryOptions repositoryOptions;

    public AmazonDownloadsList(AmazonServices amazonServices, AmazonRepositoryFactory.RepositoryOptions repositoryOptions) {
        this.amazonDynamoDB = amazonServices.getAmazonDynamoDB();
        this.repositoryOptions = repositoryOptions;
    }



    @Override
    public List<DownloadDescriptor> getDownloads(Options options) {

        long time = System.currentTimeMillis();

        QueryRequest queryRequest = AmazonDownloadEntity.queryRequestByPackageAndDate(options.get(Options.PACKAGE_NAME),
                Long.valueOf(options.get(Options.UPDATE_TIME))).withTableName(repositoryOptions.downloadTable);

        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<DownloadDescriptor> descriptors = new ArrayList<DownloadDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            descriptors.add(AmazonDownloadEntity.getDescriptor(item));
        }
        System.out.println("List downloads time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return descriptors;
    }
}
