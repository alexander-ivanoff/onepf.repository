package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.api.responsewriter.descriptors.PurchaseDescriptor;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * Provide get purchases request to underlying DataService
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetPurchasesRequestHandler extends BaseRequestHandler {

    private final Logger logger = LogManager.getLogger(GetPurchasesRequestHandler.class.getName());


    public GetPurchasesRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    /**
     * return list of purchases for given packageName and page
     *
     * @param packageName - name of the package
     * @param pageHash - hash of the page to return
     * @return List of PurchaseDescriptor
     * @throws DataException
     */
    public List<PurchaseDescriptor> getPurchases(String packageName, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<PurchaseDescriptor> purchases = dataService.getPurchases(packageName, pageHash);
        logger.debug("List purchases time: {}", (System.currentTimeMillis() - time));
        return purchases;
    }


}
