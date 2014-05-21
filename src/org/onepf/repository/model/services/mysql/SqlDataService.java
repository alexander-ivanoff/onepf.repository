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
import org.onepf.repository.model.services.Error;
import org.onepf.repository.utils.Utils;

import javax.sql.DataSource;
import java.util.*;

/**
 * MySQL database wrapper.
 *
 * @author Alexander Ivanov
 */
public class SqlDataService implements DataService {

    // TODO refactoring: move method in different requests (Maybe Entities), here should be only generic requests

    private final Logger alarmCauseLogger = LogManager.getLogger("AlarmCauseLogger");

    private static final int PAGE_LIMIT_APPLICATIONS = 50;
    private static final int PAGE_LIMIT_OTHER = 50;

    private static final int DEFAULT_RESULT_LIMIT = 1000;

    private static final String AUTH_TOKEN = "authToken";

    private static final Logger logger = LogManager.getLogger(SqlDataService.class.getName());


    private DataSource dbDataSource;

    private SessionFactory hibSessionFactory;

    public SqlDataService(SqlOptions options) {
        hibSessionFactory = setupHibernateSessionFactory(options);

    }

    public static SessionFactory setupHibernateSessionFactory(SqlOptions options) {
        Configuration configuration = new Configuration();
        configuration.configure(options.hibernateSettingFile);
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
        saveOrUpdateEntity(lastUpdate);
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
        Query query = session.createQuery("FROM DownloadEntity WHERE homeStoreId= :homeStoreIdParam AND currPageHash= :currPageHashParam ORDER BY id DESC");
        query.setParameter("homeStoreIdParam", homeStoreId);
        if (currPageHash >= 0) {
            query.setParameter("currPageHashParam", currPageHash);
        } else {
            Query currPageHashQuery = session.createQuery("FROM DownloadEntity WHERE homeStoreId= :homeStoreIdParam ORDER BY id DESC");
            currPageHashQuery.setParameter("homeStoreIdParam", homeStoreId);
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
            Query currPageHashQuery = session.createQuery("FROM PurchaseEntity WHERE homeStoreId= :homeStoreIdParam ORDER BY id DESC");
            currPageHashQuery.setParameter("homeStoreIdParam", homeStoreId);
            currPageHashQuery.setMaxResults(1);
            List list = currPageHashQuery.list();
            if (list != null && !list.isEmpty()) {
                BaseHashEntity purchase = (BaseHashEntity) list.get(0);
                query.setParameter("currPageHashParam", purchase.getCurrPageHash());
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
        Query query = session.createQuery("FROM AppstoreEntity as appstore " +
                "INNER JOIN appstore.appstoreId  WHERE homeStoreId= :homeStoreIdParam AND currPageHash= :currPageHashParam ORDER BY id DESC");
        query.setParameter("homeStoreIdParam", homeStoreId);
        if (currPageHash >= 0) {
            query.setParameter("currPageHashParam", currPageHash);
        } else {
            Query currPageHashQuery = session.createQuery("FROM ReviewEntity WHERE homeStoreId= :homeStoreIdParam ORDER BY id DESC");
            currPageHashQuery.setParameter("homeStoreIdParam", homeStoreId);
            currPageHashQuery.setMaxResults(1);
            List list = currPageHashQuery.list();
            if (list != null && !list.isEmpty()) {
                BaseHashEntity hashEntity = (BaseHashEntity) list.get(0);
                query.setParameter("currPageHashParam", hashEntity.getCurrPageHash());
            } else {
                return new ArrayList<ReviewEntity>();
            }
        }
        query.setMaxResults(DEFAULT_RESULT_LIMIT);
        List list = query.list();
        session.close();
        return new ArrayList<ReviewEntity>(list);
    }

    @Override
    public AppstoreEntity getHomeStore(String packageName) throws DataException {
        Session session = null;
        AppstoreEntity appstore = null;
        try {
            session = getSession();
            ApplicationEntity applicationEntity =  (ApplicationEntity) session.createQuery("FROM ApplicationEntity where packageName = :packageParam")
                    .setParameter("packageParam", packageName).uniqueResult();
            if (applicationEntity == null) {
                throw new DataException(Error.BAD_REQUEST.withMessage("package not found"));
            }
            appstore =  (AppstoreEntity) session.createQuery("FROM AppstoreEntity where appstoreId = :appstoreIdParam")
                    .setParameter("appstoreIdParam", applicationEntity.getAppstoreId()).uniqueResult();
        } catch (RuntimeException e) {
            throw new DataException(Error.INTERNAL_ERROR);
        } finally {
            if (session != null) {
                session.close();
            }
        }
        return appstore;
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
        entity.setCurrPageHash(pageHashes.fst);
        entity.setPrevPageHash(pageHashes.snd);
        saveEntity(entity);
    }


    /**
     * @param homestoreId is used in downloads, purchases, reviews
     * @param limit       number of the records per one page
     * @return pair <currPageHash, prevPageHash>
     * @throws java.sql.SQLException
     */
    private Pair<Integer, Integer> getPageHashes(BaseHashEntity entity, String homestoreId, int limit) {
        Session session = getSession();
        int chash = 0, phash = 0;
        StringBuilder subQueryBuilder = new StringBuilder(String.format("FROM %s", entity.getClass().getName()));
        if (homestoreId != null) {
            subQueryBuilder.append(" WHERE homeStoreId = :homeStoreIdParam");
        }
        subQueryBuilder.append(" ORDER BY id DESC");

        Query query = session.createQuery(subQueryBuilder.toString());
        if (homestoreId != null) {
            query.setParameter("homeStoreIdParam", homestoreId);
        }
        query.setMaxResults(1); //todo try uniqueObject()
        List list = query.list();
        if (list != null && !list.isEmpty()) {
            BaseHashEntity baseEntity = (BaseHashEntity) list.get(0);
            chash = baseEntity.getCurrPageHash();
            phash = baseEntity.getPrevPageHash();
        }
        Query hashQuery = session.createQuery(String.format("FROM %s WHERE currPageHash = :currPageHashParam", entity.getClass().getName()));
        hashQuery.setParameter("currPageHashParam", chash);
        List hashQueryList = hashQuery.list();
        session.close();
        int count = 0;

        if (hashQueryList != null) {
            count = hashQueryList.size();
        }
        if (count >= limit) {
            phash = chash;
            chash += 1;
        }
        return new Pair<Integer, Integer>(chash, phash);
    }

    public void saveStatisticEntity(BaseStatisticEntity statisticEntity, LastStatisticsUpdateEntity lastUpdateEntity) throws DataException{
        Session session = null;
        try {
            ApplicationEntity app = getApplicationsLog(statisticEntity.getPackageName(), -1).get(0);
            // if application not found we
            if (app != null) {
                // add homeStoreId to statistics
                statisticEntity.setHomeStoreId(app.getAppstoreId());
                // add page number to statistics
                Pair<Integer, Integer> pageHashes = getPageHashes(statisticEntity, statisticEntity.getHomeStoreId(), PAGE_LIMIT_OTHER);
                statisticEntity.setCurrPageHash(pageHashes.fst);
                statisticEntity.setPrevPageHash(pageHashes.snd);
                session = getSession();
                session.beginTransaction();
                session.save(statisticEntity);
            } else {
                alarmCauseLogger.error("Failed to save statistics. Package {} not found!", statisticEntity.getPackageName());
            }
            lastUpdateEntity.setLastResponseCount(lastUpdateEntity.getLastResponseCount() + 1);
            lastUpdateEntity.setLastResponseDatetime(Utils.sqlFormattedDate(new Date(System.currentTimeMillis())));
            session.saveOrUpdate(lastUpdateEntity);
            session.getTransaction().commit();
        } catch (RuntimeException e) {
            session.getTransaction().rollback();
            throw new DataException(e);
        } finally {
            if (session != null) {
                session.close();
            }
        }

    }

    public void saveEntity(BaseEntity entity) {
        Session session = getSession();
        session.beginTransaction();
        session.save(entity);
        session.getTransaction().commit();
        session.close();
    }

    public void saveOrUpdateEntity(BaseEntity entity) {
        Session session = getSession();
        session.beginTransaction();
        session.saveOrUpdate(entity);
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
