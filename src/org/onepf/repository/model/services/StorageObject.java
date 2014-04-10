package org.onepf.repository.model.services;

import java.io.InputStream;

/**
 * Abstraction describes file to download
 *
 * @author Alexander Ivanoff
 */
public interface StorageObject {

    /**
     * @return - input stream to get content from
     * @throws StorageException
     *
     * @see org.onepf.repository.model.services.StorageException
     */
    InputStream asStream() throws StorageException;

    /**
     *
     * @return  - length of the content stored in service
     * @throws StorageException
     *
     * @see org.onepf.repository.model.services.StorageException
     */
    long size() throws StorageException;
}
