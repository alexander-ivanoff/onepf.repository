package org.onepf.repository;

import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.api.uploader.NoMultipartException;
import org.onepf.repository.api.uploader.UploadFileHandler;
import org.onepf.repository.model.UploadAppdfRequestHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.util.Map;

/**
 *
 * This Servlet handle upload of Appdf file to the server.
 *
* @author Alexander Ivanoff on 11.03.14.
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
            AppstoreEntity appstore = getAuthenticator().getAuthorizedAppstore(formFields);
            if (appstore != null) {
                for (File file : appdfFileUploder.getUploadedFiles()) {
                    appdfHandler.processFile(file, null, appstore);
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
