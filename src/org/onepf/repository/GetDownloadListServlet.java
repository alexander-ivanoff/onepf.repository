package org.onepf.repository;

import org.onepf.repository.api.responsewriter.ResponseWriterV2;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.api.xmlapi.XmlResponseWriterV2;
import org.onepf.repository.model.services.SimpleListRequestHandler;

import java.util.List;

/**
 *
 * This Servlet returns list of all packages stored in repository.
 *
* @author Alexander Ivanoff on 11.03.14.
 */
public class GetDownloadListServlet extends SimpleListServlet<DownloadEntity, DownloadListEntity> {


    @Override
    String initOffsetTemplate() {
        return "downloads_%d.xml";
    }

    @Override
    ResponseWriterV2 initResponseWriter() {
        try {
            return new XmlResponseWriterV2(ObjectFactory._Downloads_QNAME, DownloadListEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    SimpleListRequestHandler<DownloadEntity> initRequestHandler() {
        return getRepositoryFactory().createDownloadsHandler();
    }

    @Override
    public DownloadListEntity buildListEntity(List<DownloadEntity> entities) {
        DownloadListEntity listEntity = new DownloadListEntity();
        listEntity.getDownload().addAll(entities);
        return listEntity;
    }
}
