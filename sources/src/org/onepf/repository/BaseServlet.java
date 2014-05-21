package org.onepf.repository;

import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.model.RepositoryConfigurator;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.services.DataException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 *
 * Base Class for all Servlets. Handle authorization process.
 *
* @author Alexander Ivanoff on 14.03.14.
 */
public abstract class BaseServlet extends HttpServlet {

    private AppstoreAuthenticator authenticator;
    private RepositoryFactory factory;


    public void init() throws ServletException{
        authenticator = RepositoryConfigurator.getAppstoreAuthenticator(getServletContext());
        factory = RepositoryConfigurator.getRepositoryFactory(getServletContext());
    }

    protected RepositoryFactory getRepositoryFactory() {
        return factory;
    }

    protected AppstoreAuthenticator getAuthenticator() {
        return authenticator;
    }

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        try {
            if (authenticator.isAuthorized(req)) {
                get(req, resp);
            } else {
                resp.sendError(HttpServletResponse.SC_UNAUTHORIZED);
            }
        } catch (DataException e) {
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        try {
            if (authenticator.isAuthorized(req)) {
                post(req, resp);
            } else {
                resp.sendError(HttpServletResponse.SC_UNAUTHORIZED);
            }
        } catch (DataException e) {
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    protected AppstoreEntity getAppstore(HttpServletRequest req) throws DataException{
        return authenticator.getAuthorizedAppstore(req);
    }

    abstract void get(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException;

    abstract void post(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException;
}
