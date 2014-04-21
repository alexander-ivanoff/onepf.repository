package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseReaderWriter;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.responsewriter.entity.ApplicationListEntity;
import org.onepf.repository.api.responsewriter.entity.ObjectFactory;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;
import org.onepf.repository.model.services.SimpleListRequestHandler;

import java.util.List;

/**
 *
 * This Servlet returns list of all packages stored in repository.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class GetApplicationsListServlet extends SimpleListServlet<ApplicationEntity, ApplicationListEntity> {


    @Override
    String initOffsetTemplate() {
        return "applist_%d.xml";
    }

    @Override
    ResponseReaderWriter initResponseWriter() {
        try {
            return new XmlResponseReaderWriter(ObjectFactory._ApplicationList_QNAME, ApplicationListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    SimpleListRequestHandler<ApplicationEntity> initRequestHandler() {
        return getRepositoryFactory().createApplicationsHandler();
    }

    @Override
    public ApplicationListEntity buildListEntity(List<ApplicationEntity> entities) {
        ApplicationListEntity listEntity = new ApplicationListEntity();
        listEntity.getApplication().addAll(entities);
        return  listEntity;
    }
}
