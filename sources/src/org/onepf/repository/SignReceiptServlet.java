package org.onepf.repository;

import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.model.SignReceiptHandler;
import org.onepf.repository.model.services.DataException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedReader;
import java.io.IOException;

/**
 *
 * This Servlet returns list of all packages stored in repository.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class SignReceiptServlet extends BaseServlet {

    private SignReceiptHandler handler;

    @Override
    public synchronized void init() throws ServletException {
        super.init();
       handler =  getRepositoryFactory().createSignReceiptHandler();
    }

    @Override
    void get(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {

    }

    @Override
    void post(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {

        AppstoreEntity distributorAppstore = null;
        try {
            distributorAppstore = getAppstore(req);
        } catch (DataException e) {
            resp.sendError(HttpServletResponse.SC_UNAUTHORIZED);
        }

        StringBuffer body = new StringBuffer();
        String line = null;
        try {
            BufferedReader reader = req.getReader();
            while ((line = reader.readLine()) != null)
                body.append(line);
        } catch (Exception e) {
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }


        if (body != null && distributorAppstore != null) {
            try {
                handler.getSignedReceipt(distributorAppstore, body.toString(), resp.getOutputStream());
            } catch (DataException e) {
                switch (e.getError()) {
                    case GENERIC:
                    case INTERNAL_ERROR:
                        resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
                    case BAD_REQUEST:
                        resp.sendError(HttpServletResponse.SC_BAD_REQUEST, e.getMessage());
                }
            }
        }

    }

}