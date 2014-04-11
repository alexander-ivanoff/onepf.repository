package org.onepf.repository.model.services;

import java.io.InputStream;

/**
 * Abstraction of service to store files
 *
 * @see org.onepf.repository.model.services.filesystem.FilesystemStorageService
 * @see org.onepf.repository.model.services.amazon.AmazonStorageService
 * @author Alexander Ivanoff
 */
public interface StorageService {

    /**
     * Store object to storage service
     *
     * @param objectKey - key in storage service to access stored file
     * @param is - input stream to get content from
     * @param contentLength - length of the content to store in service
     * @throws StorageException
     *
     * @see org.onepf.repository.model.services.StorageException
     */
    void storeObject(String objectKey, InputStream is, long contentLength) throws StorageException;

    /**
     * Method to get object describes file to download
     *
     * @param objectKey - key in storage service to access stored file
     * @return - object describes file to download
     * @throws StorageException
     *
     * @see org.onepf.repository.model.services.StorageObject
     * @see org.onepf.repository.model.services.StorageException
     */
    StorageObject getObject(String objectKey) throws StorageException;

}
