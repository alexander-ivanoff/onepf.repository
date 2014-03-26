package org.onepf.repository.model.services.filesystem;

import org.onepf.repository.model.FileType;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageService;

import java.io.InputStream;

/**
 * Created by ivanoff on 26.03.14.
 */
public class FilesystemStorageService implements StorageService {
    @Override
    public void storeObject(String packageName, InputStream is, FileType fileType, long contentLength) throws StorageException {

    }

    @Override
    public InputStream getObjectAsStream(String packageName, FileType fileType) throws StorageException {
        return null;
    }

    @Override
    public long getObjectSize(String packageName, FileType fileType) throws StorageException {
        return 0;
    }
}
