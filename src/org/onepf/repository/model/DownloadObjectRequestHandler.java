package org.onepf.repository.model;

import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageObject;
import org.onepf.repository.model.services.StorageService;

/**
 * Created by ivanoff on 12.03.14.
 */
public class DownloadObjectRequestHandler extends BaseRequestHandler {

    public static class ObjectOptions {
        public String packageName;
        public FileType fileType = FileType.APPDF;

    }

    public DownloadObjectRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public StorageObject getObject(ObjectOptions options) throws StorageException{
        return storageService.getObject(options.packageName, options.fileType);
    }
}
