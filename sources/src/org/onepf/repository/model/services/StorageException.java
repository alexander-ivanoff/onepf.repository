package org.onepf.repository.model.services;

/**
 * This exception can be thrown by StorageObject or StorageService.
 *
 * @see org.onepf.repository.model.services.StorageObject
 * @see org.onepf.repository.model.services.StorageService
 *
* @author Alexander Ivanoff on 25.03.14.
 */
public class StorageException extends Exception {

    public StorageException(Exception e) {
        super(e);
    }
}
