package org.onepf.repository;

import org.onepf.repository.model.UploadAppdfRequestHandler;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.utils.uploader.NoMultipartException;
import org.onepf.repository.utils.uploader.UploadFileHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.util.Map;

/**
 * Created by ivanoff on 11.03.14.
 */
public class UploadAppdfFileServlet extends BaseServlet {

    private static final String DEVELOPERS_CONTACT = "developersContact";

    private UploadAppdfRequestHandler appdfHandler;

    File tempDir;
    File uploadDir;

    @Override
    public void init() throws ServletException {
        super.init();

        tempDir = (File)getServletContext().getAttribute("javax.servlet.context.tempdir");
        uploadDir = new File(getServletContext().getRealPath("/uploads/")); // Move to RepositoryOptions

        appdfHandler = getRepositoryFactory().createAppDFFileHandler();

    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        UploadFileHandler appdfFileUploder = new UploadFileHandler(tempDir, uploadDir);
        appdfFileUploder.createUploadDirIfNotExist();
        Map<String, String> formFields = appdfFileUploder.getFormFields();
        try {
            appdfFileUploder.upload(request);
            AppstoreDescriptor appstore = getAuthenticator().getAuthorizedAppstore(formFields);
            if (appstore != null) {
                for (File file : appdfFileUploder.getUploadedFiles()) {
                    appdfHandler.processFile(file, formFields.get(DEVELOPERS_CONTACT), appstore);
                }
            } else {
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED);
            }

        } catch (NoMultipartException e) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST);
        } catch (Exception e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } finally {
            appdfFileUploder.cleanup();
        }
    }

    @Override
    void post(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {

    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }

}
