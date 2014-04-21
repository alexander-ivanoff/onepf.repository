package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseReaderWriter;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.BaseEntity;
import org.onepf.repository.api.responsewriter.entity.BaseListEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.SimpleListRequestHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 *
 * This Servlet returns list of all packages stored in repository.
 *
 * @author Alexander Ivanoff on 11.03.14.
 */
public abstract class SimpleListServlet<T extends BaseEntity, K extends BaseListEntity> extends BaseServlet {

    private static final String PARAMETER_PAGE = "page";

    private String fileTemplate;
    private ResponseReaderWriter responseWriter;
    private SimpleListRequestHandler<T> handler;


    abstract String initOffsetTemplate();
    abstract ResponseReaderWriter initResponseWriter();
    abstract SimpleListRequestHandler<T> initRequestHandler();

    @Override
    public void init() throws ServletException {
        super.init();
        fileTemplate = initOffsetTemplate();
        responseWriter = initResponseWriter();
        handler = initRequestHandler();
    }

    protected void post(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    }

    protected void get(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String page = request.getParameter(PARAMETER_PAGE);
        try {
            List<T> apps = handler.getList(getAppstore(request).getAppstoreId(), page != null ? Integer.valueOf(page) : -1);
            String offset = getOffset(request, apps);
            K listEntity = buildListEntity(apps);
            listEntity.setVersion("1");
            listEntity.setOffset(offset);
            responseWriter.write(response.getOutputStream(), listEntity);
        } catch (WriteException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } catch (DataException e) {
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    protected String getOffset(HttpServletRequest request, List<T> entities) {
        String offset = null;
        if (entities.size() > 0 ) {
            T lastApp = entities.get(0);
            if (lastApp.getCurrPageHash() != lastApp.getPrevPageHash()) {
                String  url = request.getRequestURL().toString();
                url = url.substring(0, url.lastIndexOf('/'));
                offset = url + '/' + String.format(fileTemplate, entities.get(0).getPrevPageHash());
            }
        }
        return offset;
    }

    abstract public K buildListEntity(List<T> entities);

}