package org.onepf.repository.api.uploader;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.util.*;

/**
 * Created by ivanoff on 11.03.14.
 */
public class UploadFileHandler {

    private Random random = new Random();

    private static final int SIZE_THRESOLD = 1024*1024;
    private static final int SIZE_MAX = 1024*1024*100;

    private File uploadDir;
    private File tempDir;

    private List<File> uploadedFiles = new ArrayList<File>();
    private Map<String, String> formFields = new HashMap<String, String>();


    public UploadFileHandler(File tempDir, File uploadDir) {
        this.tempDir = tempDir;
        this.uploadDir = uploadDir;
    }

    public boolean createUploadDirIfNotExist() {
        return uploadDir.mkdirs();
    }

    public void upload(HttpServletRequest request) throws Exception {
        cleanup();

        boolean isMultipart = ServletFileUpload.isMultipartContent(request);
        if (!isMultipart) {
            throw new NoMultipartException();
        }

        DiskFileItemFactory factory = new DiskFileItemFactory();
        factory.setSizeThreshold(SIZE_THRESOLD);
        factory.setRepository(tempDir);

        ServletFileUpload upload = new ServletFileUpload(factory);
        upload.setSizeMax(SIZE_MAX);

        List<FileItem> items = upload.parseRequest(request);
        for (FileItem item: items) {
            if (item.isFormField()) {
                formFields.put(item.getFieldName(), item.getString());
            } else {
                copyToUploadDir(item);
            }
        }
    }

    private void copyToUploadDir(FileItem item) throws Exception {
        File uploadedFile = null;
        do{
            uploadedFile = new File(uploadedFile, random.nextInt() + "_" + item.getName());
        } while(uploadedFile.exists());

        uploadedFile.createNewFile();
        item.write(uploadedFile);

        uploadedFiles.add(uploadedFile);
    }

    public List<File> getUploadedFiles() {
        return uploadedFiles;
    }

    public Map<String, String> getFormFields() {
        return formFields;
    }

    public void cleanup() {
        for (File file : uploadedFiles) {
            file.delete();
        }
        formFields.clear();
        uploadedFiles.clear();
    }


}
