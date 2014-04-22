package org.onepf.repository.model.services.mysql;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.boot.registry.StandardServiceRegistry;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.cfg.Configuration;
import org.onepf.repository.api.Pair;
import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.appstorelooter.FeedType;
import org.onepf.repository.appstorelooter.LastStatisticsUpdateEntity;
import org.onepf.repository.appstorelooter.LastUpdateEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;

import javax.sql.DataSource;
import java.util.*;

/**
 * MySQL database wrapper.
 *
 * @author Alexander Ivanov
 */
public class SqlDataService implements DataService {

    // TODO refactoring: move method in different requests (Maybe Entities), here should be only generic requests

    private static final int PAGE_LIMIT_APPLICATIONS = 50;
    private static final int PAGE_LIMIT_OTHER = 50;

    private static final int DEFAULT_RESULT_LIMIT = 1000;

    private static final String AUTH_TOKEN = "authToken";

    private static final Logger logger = LogManager.getLogger(SqlDataService.class.getName());


    private DataSource dbDataSource;

    private SessionFactory hibSessionFactory;

    public SqlDataService(SqlOptions options) {
        hibSessionFactory = setupHibernateSessionFactory();

    }

    public static SessionFactory setupHibernateSessionFactory() {
        Configuration configuration = new Configuration();
        configuration.configure("/resources/hibernate.cfg.xml");
        StandardServiceRegistry serviceRegistry = new StandardServiceRegistryBuilder().applySettings(
                configuration.getProperties()).build();
        return configuration.buildSessionFactory(serviceRegistry);
    }


    @Override
    public void store(ApplicationEntity appEntity) throws DataException {
        insertWithHashes(appEntity, PAGE_LIMIT_APPLICATIONS);

    }

    @Override
    public void saveLastUpdate(LastUpdateEntity lastUpdate) throws DataException {
        saveEntity(lastUpdate);
    }

    @Override
    public void saveLastStatisticsUpdate(LastStatisticsUpdateEntity lastStatisticsUpdate) throws DataException {
        saveEntity(lastStatisticsUpdate);
    }

    @Override
    public void addDownload(DownloadEntity download) throws DataException {
        insertWithHashes(download.getPackageName(), download, PAGE_LIMIT_OTHER);
    }

    @Override
    public List<ApplicationEntity> getApplicationsLog() throws DataException {
        return getApplicationsLog(null, -1);
    }


    @Override
    public List<ApplicationEntity> getApplicationsLog(String packageName, int currPageHash) throws DataException {
        Session session = getSession();

        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("FROM ApplicationEntity");
        List<Pair<String, Object>> params = new ArrayList<Pair<String, Object>>();
        if (packageName != null && packageName.length() > 0) {
            stringBuilder.append(" WHERE packageName = :packageParam");
            params.add(new Pair<String, Object>("packageParam", packageName));
        } else {
            int pageHash = currPageHash;
            if (pageHash >= 0) {
                stringBuilder.append(" WHERE currPageHash = :currPageHashParam");
                params.add(new Pair<String, Object>("currPageHashParam", currPageHash));
            } else {
                String hqlSelection = "FROM ApplicationEntity ORDER BY id DESC";
                Query query = session.createQuery(hqlSelection);
                query.setMaxResults(1);
                List list = query.list();
                if (!list.isEmpty()) {
                    pageHash = ((ApplicationEntity) list.get(0)).getCurrPageHash();
                    stringBuilder.append(" WHERE currPageHash = :currPageHashParam");
                    params.add(new Pair<String, Object>("currPageHashParam", pageHash));
                } else {
                    session.close();
                    return new ArrayList<ApplicationEntity>();
                }
            }
        }

        Query query = session.createQuery(stringBuilder.toString());
        if (params.size() > 0) {
            for (Pair<String, Object> pair : params) {
                query.setParameter(pair.fst, pair.snd);
            }

        }
        List results = query.list();
        session.close();
        return results;
    }


    @Override
    public List<ApplicationEntity> getApplicationByHash(String packageName, String hash) throws DataException {
        Session session = getSession();
        Query query = session.createQuery("FROM ApplicationEntity where packageName = :packageParam and appdfHash = :hashParam ORDER BY id DESC");
        query.setParameter("packageParam", packageName);
        query.setParameter("hashParam", hash);
        query.setMaxResults(1);
        List list = query.list();
        session.close();
        return list;
    }


    @Override
    public Map<String, AppstoreEntity> getAppstores() throws DataException {
        Session session = getSession();
        Query query = session.createQuery("FROM AppstoreEntity");
        query.setMaxResults(DEFAULT_RESULT_LIMIT);
        List list = query.list();
        session.close();
        Map<String, AppstoreEntity> appstoreEntityMap = new HashMap<String, AppstoreEntity>();
        for (Object object : list) {
            AppstoreEntity appstoreEntity = (AppstoreEntity) object;
            appstoreEntityMap.put(appstoreEntity.getRepositoryAccessToken(), appstoreEntity);
        }
        return appstoreEntityMap;
    }

    @Override
    public LastUpdateEntity getLastUpdate(String appstoreId) throws DataException {
        Session session = getSession();
        Query query = session.createQuery("FROM LastUpdateEntity WHERE appstoreId = :appstoreIdParam");
        query.setParameter("appstoreIdParam", appstoreId);
        query.setMaxResults(1);
        List results = query.list();
        session.close();
        if (results != null && results.size() == 1) {
            return (LastUpdateEntity) results.get(0);
        } else {
            return null;
        }
    }


    @Override
    public LastStatisticsUpdateEntity getLastStatisticsUpdate(String appstoreId, FeedType feedType) throws DataException {
        Session session = getSession();
        Query query = session.createQuery("FROM LastStatisticsUpdateEntity WHERE appstoreId = :appstoreIdParam AND feedType = :feedTypeParam");
        query.setParameter("appstoreIdParam", appstoreId);
        query.setParameter("feedTypeParam", feedType);
        query.setMaxResults(1);
        List results = query.list();
        session.close();
        if (results != null && results.size() == 1) {
            return (LastStatisticsUpdateEntity) results.get(0);
        } else {
            return null;
        }
    }

    @Override
    public ArrayList<DownloadEntity> getDownloads(String homeStoreId, long currPageHash) throws DataException {
        Session session = getSession();
        Query query = session.createQuery("FROM PurchaseEntity WHERE homeStoreId= :homeStoreIdParam AND currPageHash= :currPageHashParam ORDER BY id DESC");
        query.setParameter("homeStoreIdParam", homeStoreId);
        if (currPageHash >= 0) {
            query.setParameter("currPageHashParam", currPageHash);
        } else {
            Query currPageHashQuery = session.createQuery("FROM DownloadEntity WHERE homeStoreId= :homeStoreIdParam ORDER BY id DESC");
            currPageHashQuery.setMaxResults(1);
            List list = currPageHashQuery.list();
            if (list != null && !list.isEmpty()) {
                DownloadEntity download = (DownloadEntity) list.get(0);
                query.setParameter("currPageHashParam", download.getCurrPageHash());
            } else {
                return new ArrayList<DownloadEntity>();
            }
        }
        query.setMaxResults(DEFAULT_RESULT_LIMIT);
        List list = query.list();
        session.close();
        return new ArrayList<DownloadEntity>(list);
    }

    @Override
    public ArrayList<PurchaseEntity> getPurchases(String homeStoreId, long currPageHash) throws DataException {
        Session session = getSession();
        Query query = session.createQuery("FROM PurchaseEntity WHERE homeStoreId= :homeStoreIdParam AND currPageHash= :currPageHashParam ORDER BY id DESC");
        query.setParameter("homeStoreIdParam", homeStoreId);
        if (currPageHash >= 0) {
            query.setParameter("currPageHashParam", currPageHash);
        } else {
            Query currPageHashQuery = session.createQuery("FROM DownloadEntity WHERE homeStoreId= :homeStoreIdParam ORDER BY id DESC");
            currPageHashQuery.setMaxResults(1);
            List list = currPageHashQuery.list();
            if (list != null && !list.isEmpty()) {
                DownloadEntity download = (DownloadEntity) list.get(0);
                query.setParameter("currPageHashParam", download.getCurrPageHash());
            } else {
                return new ArrayList<PurchaseEntity>();
            }
        }
        query.setMaxResults(DEFAULT_RESULT_LIMIT);
        List list = query.list();
        session.close();
        return new ArrayList<PurchaseEntity>(list);
    }

    @Override
    public ArrayList<ReviewEntity> getReviews(String homeStoreId, long currPageHash) throws DataException {
        Session session = getSession();
        Query query = session.createQuery("FROM ReviewEntity WHERE homeStoreId= :homeStoreIdParam AND currPageHash= :currPageHashParam ORDER BY id DESC");
        query.setParameter("homeStoreIdParam", homeStoreId);
        if (currPageHash >= 0) {
            query.setParameter("currPageHashParam", currPageHash);
        } else {
            Query currPageHashQuery = session.createQuery("FROM DownloadEntity WHERE homeStoreId= :homeStoreIdParam ORDER BY id DESC");
            currPageHashQuery.setMaxResults(1);
            List list = currPageHashQuery.list();
            if (list != null && !list.isEmpty()) {
                DownloadEntity download = (DownloadEntity) list.get(0);
                query.setParameter("currPageHashParam", download.getCurrPageHash());
            } else {
                return new ArrayList<ReviewEntity>();
            }
        }
        query.setMaxResults(DEFAULT_RESULT_LIMIT);
        List list = query.list();
        session.close();
        return new ArrayList<ReviewEntity>(list);
    }

    /**
     * insert new record to table with paging (add page hashes to insert)
     */

    private void insertWithHashes(BaseHashEntity entity, int limit) {
        insertWithHashes(null, entity, limit);
    }

    /**
     * insert new record to table with paging (add page hashes to insert)
     */
    private void insertWithHashes(String packageName, BaseHashEntity entity, int limit) {
        Pair<Integer, Integer> pageHashes = getPageHashes(entity, packageName, limit);
        entity.setPrevPageHash(pageHashes.fst);
        entity.setCurrPageHash(pageHashes.snd);
        saveEntity(entity);
    }


    /**
     * @param packageName is used in downloads, purchases, reviews
     * @param limit       number of the records per one page
     * @return pair <currPageHash, prevPageHash>
     * @throws java.sql.SQLException
     */
    private Pair<Integer, Integer> getPageHashes(BaseHashEntity entity, String packageName, int limit) {
        Session session = getSession();
        int currentPageHashParam = 0;
        StringBuilder subQueryBuilder = new StringBuilder("FROM ApplicationEntity");
        if (packageName != null) {
            subQueryBuilder.append(" WHERE packageName = :packageNameParam");
        }
        subQueryBuilder.append(" ORDER BY id DESC");

        Query query = session.createQuery(subQueryBuilder.toString());
        if (packageName != null) {
            query.setParameter("packageNameParam", packageName);
        }
        query.setMaxResults(1); //todo try uniqueObject()
        List list = query.list();
        if (list != null && !list.isEmpty()) {
            ApplicationEntity applicationEntity = (ApplicationEntity) list.get(0);
            currentPageHashParam = applicationEntity.getCurrPageHash();
        }
        Query hashQuery = session.createQuery(String.format("FROM %s WHERE currPageHash = :currPageHashParam", entity.getClass().getName()));
        hashQuery.setParameter("currPageHashParam", currentPageHashParam);
        List hashQueryList = hashQuery.list();
        session.close();
        int count = 0;
        int chash = 0, phash = 0;
        if (hashQueryList != null) {
            count = hashQueryList.size();
            ArrayList<BaseHashEntity> hashEntities = new ArrayList<BaseHashEntity>(hashQueryList);
            for (BaseHashEntity hashEntity : hashEntities) {
                chash = hashEntity.getCurrPageHash();
                phash = hashEntity.getPrevPageHash();
            }
        }
        if (count >= limit) {
            phash = chash;
            chash += 1;
        }
        return new Pair<Integer, Integer>(chash, phash);
    }


    public void saveEntity(BaseEntity entity) {
        Session session = getSession();
        session.beginTransaction();
        session.save(entity);
        session.getTransaction().commit();
        session.close();
    }

    @Override
    public void close() {
        if (hibSessionFactory != null) {
            hibSessionFactory.close();
        }
    }

    public Session getSession() {
        return hibSessionFactory.openSession();
    }
}
