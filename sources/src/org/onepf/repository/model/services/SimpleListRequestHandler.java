package org.onepf.repository.model.services;

import org.onepf.repository.api.responsewriter.entity.BaseEntity;
import org.onepf.repository.model.BaseRequestHandler;

import java.util.List;

/**
 *  Provide get applications request to underlying DataService
* @author Alexander Ivanoff on 12.03.14.
 */
public abstract class SimpleListRequestHandler<T extends BaseEntity> extends BaseRequestHandler {

    public SimpleListRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    /**
     * return list of Entities for given page
     *
     * @param appstoreId - id of requesting store
     * @param pageHash - hash of the page to return
     * @return List of Entities
     * @throws org.onepf.repository.model.services.DataException
     */
    public abstract List<T> getList(String appstoreId, int pageHash) throws DataException;
}
