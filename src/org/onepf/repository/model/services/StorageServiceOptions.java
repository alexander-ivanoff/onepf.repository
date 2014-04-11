package org.onepf.repository.model.services;

/**
 * Abstraction of options object to create and configure storage service
 *
* @author Alexander Ivanoff
 */
public interface StorageServiceOptions {

    /**
     * Create Right Options object. Must be overriden in childs.
     *
     * @return created storage service
     *
     * @see org.onepf.repository.model.services.StorageService
     */
    StorageService createStorageService();
}
