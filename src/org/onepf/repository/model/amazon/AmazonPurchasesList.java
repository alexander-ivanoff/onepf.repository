package org.onepf.repository.model.amazon;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import com.amazonaws.services.dynamodbv2.model.QueryResult;
import org.onepf.repository.model.Options;
import org.onepf.repository.model.PurchasesList;
import org.onepf.repository.model.amazon.db.AmazonPurchaseEntity;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 12.03.14.
 */
public class AmazonPurchasesList extends PurchasesList {

    private AmazonDynamoDB amazonDynamoDB;

    private AmazonRepositoryFactory.RepositoryOptions repositoryOptions;

    public AmazonPurchasesList(AmazonServices amazonServices, AmazonRepositoryFactory.RepositoryOptions repositoryOptions) {
        this.amazonDynamoDB = amazonServices.getAmazonDynamoDB();
        this.repositoryOptions = repositoryOptions;
    }



    @Override
    public List<PurchaseDescriptor> getPurchases(Options options) {

        long time = System.currentTimeMillis();

        QueryRequest queryRequest = AmazonPurchaseEntity.queryRequestByPackageAndDate(options.get(Options.PACKAGE_NAME ),
                Long.valueOf(options.get(Options.UPDATE_TIME))).withTableName(repositoryOptions.purchaseTable);

        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<PurchaseDescriptor> purchases = new ArrayList<PurchaseDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            purchases.add(AmazonPurchaseEntity.getDescriptor(item));
        }
        System.out.println("List purchases time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return purchases;
    }
}
