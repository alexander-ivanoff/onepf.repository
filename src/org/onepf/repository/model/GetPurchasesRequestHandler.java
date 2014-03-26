package org.onepf.repository.model;

import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public class GetPurchasesRequestHandler extends BaseRequestHandler {


    public GetPurchasesRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public List<PurchaseDescriptor> getPurchases(String packageName, long updateTime) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<PurchaseDescriptor> purchases = dataService.getPurchases(packageName, updateTime);
        System.out.println("List purchases time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return purchases;
    }
}
