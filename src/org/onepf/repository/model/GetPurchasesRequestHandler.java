package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.PurchaseEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.SimpleListRequestHandler;
import org.onepf.repository.model.services.StorageService;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * Provide get purchases request to underlying DataService
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetPurchasesRequestHandler extends SimpleListRequestHandler<PurchaseEntity> {

    private final Logger logger = LogManager.getLogger(GetPurchasesRequestHandler.class.getName());

    public GetPurchasesRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }


    @Override
    public List<PurchaseEntity> getList(String appstoreId, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<PurchaseEntity> purchases = dataService.getPurchases(appstoreId, pageHash);
        logger.debug("List purchases time: {}", (System.currentTimeMillis() - time));
        return purchases;
    }


}
