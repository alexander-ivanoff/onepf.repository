package org.onepf.repository.model.services;

/**
 * Abstraction of options object to create and configure storage service
 *
* @author Alexander Ivanoff on 07.04.14.
 */
public interface StorageServiceOptions {

    /**
     * Create storage service with given options
     * @return created storage service
     *
     * @see org.onepf.repository.model.services.StorageService
     */
    StorageService createStorageService();
}
