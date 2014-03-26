package org.onepf.repository.model;

import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class BaseRequestHandler {

    protected DataService dataService;
    protected StorageService storageService;

    public BaseRequestHandler(DataService dataService, StorageService storageService) {
        this.dataService = dataService;
        this.storageService = storageService;
    }
}
