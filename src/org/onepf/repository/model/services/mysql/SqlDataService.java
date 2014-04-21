package org.onepf.repository.model.services.mysql;

import org.apache.commons.dbcp.ConnectionFactory;
import org.apache.commons.dbcp.DriverManagerConnectionFactory;
import org.apache.commons.dbcp.PoolableConnectionFactory;
import org.apache.commons.dbcp.PoolingDataSource;
import org.apache.commons.pool.ObjectPool;
import org.apache.commons.pool.impl.GenericObjectPool;
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
import org.onepf.repository.appstorelooter.LastUpdateEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.mysql.entities.*;

import javax.persistence.Table;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
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

    public SqlDataService(SqlOptions options) {

        // init DBCP DataSource
        dbDataSource = setupDataSource(options);

    }

    public static DataSource setupDataSource(SqlOptions options) {

        try {
            Class.forName(options.driverClassName);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        ObjectPool connectionPool = new GenericObjectPool(null, options.maxConnections, GenericObjectPool.WHEN_EXHAUSTED_BLOCK, options.maxWait);
        ConnectionFactory connectionFactory = new DriverManagerConnectionFactory(options.dbUrl, options.dbUser, options.dbPassword);
        PoolableConnectionFactory poolableConnectionFactory = new PoolableConnectionFactory(connectionFactory, connectionPool, null, null, false, true);
        PoolingDataSource dataSource = new PoolingDataSource(connectionPool);
        return dataSource;
    }

    @Override
    public void store(ApplicationEntity appEntity) throws DataException {
        Connection conn = null;
        try {
            conn = dbDataSource.getConnection();
            Table table = appEntity.getClass().getAnnotation(Table.class);
            insertWithHashes(conn, table.name(), appEntity, PAGE_LIMIT_APPLICATIONS);
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try {
                if (conn != null) conn.close();
            } catch (Exception ignored) {
            }
        }
    }

    @Override
    public void saveLastUpdate(LastUpdateEntity lastUpdate) throws DataException {
        saveEntity(lastUpdate);
    }

    @Override
    public void addDownload(DownloadEntity download) throws DataException {
        Connection conn = null;
        try {
            conn = dbDataSource.getConnection();
            Table table = download.getClass().getAnnotation(Table.class);
            insertWithHashes(conn, table.name(), download.getPackageName(), download, PAGE_LIMIT_OTHER);
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try {
                if (conn != null) conn.close();
            } catch (Exception ignored) {
            }
        }

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

    private Session getSession() {
        Configuration configuration = new Configuration();
        configuration.configure("/resources/hibernate.cfg.xml");
        StandardServiceRegistry serviceRegistry = new StandardServiceRegistryBuilder().applySettings(
                configuration.getProperties()).build();
        SessionFactory sessionFactory = configuration
                .buildSessionFactory(serviceRegistry);
        return sessionFactory.openSession();
    }

    @Override
    public List<ApplicationEntity> getApplicationByHash(String packageName, String hash) throws DataException {

        Session session = getSession();

        Query query = session.createQuery("FROM ApplicationEntity where packageName = :packageParam and appdfHash = :hashParam ORDER BY id DESC");
        query.setParameter("packageParam", packageName);
        query.setParameter("hashParam", hash);
        query.setMaxResults(1);
        session.close();
        return query.list();
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
        Query query = session.createQuery("FROM LastUpdateEntity WHERE appstoreId =: appstoreIdParam");
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


    /*@Override
    public LastStatisticsUpdateDescriptor getLastStatisticsUpdate(String appstoreId, String feedType) throws DataException{
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            conn = dbDataSource.getConnection();
            String selection = SqlLastStatisticsUpdateEntity.FIELD_APPSTORE_ID + "=? AND " + SqlLastStatisticsUpdateEntity.FIELD_FEED_TYPE + "=?";
            String[] selectionArgs = new String[] {appstoreId, feedType};
            stmt = query(conn, SqlLastStatisticsUpdateEntity.TABLE_NAME, selection, selectionArgs, null, DEFAULT_RESULT_LIMIT);
            rset = stmt.executeQuery();
            LastStatisticsUpdateDescriptor lastUpdate = null;
            if (rset.next()) {
                lastUpdate = SqlLastStatisticsUpdateEntity.getDescriptor(rset);
            }
            if (rset.next()) {
                throw new DataException("getLastStatisticsUpdate result set must contain at most one row");
            }
            return lastUpdate;
        } catch (SQLException e) {
            e.printStackTrace();
            throw new DataException(e);
        } finally {
            try { if (rset != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }*/
    @Override
    public ArrayList<DownloadEntity> getDownloads(String homeStoreId, long currPageHash) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = "homeStoreId=? AND ";
            String[] selectionArgs;
            if (currPageHash >= 0) {
                selection += "currPageHash=?";
                selectionArgs = new String[]{homeStoreId, String.valueOf(currPageHash)};
            } else {
                selection += "currPageHash = (SELECT currPageHash FROM downloads WHERE homeStoreId=? ORDER BY id DESC LIMIT 1)";
                selectionArgs = new String[]{homeStoreId, homeStoreId};
            }
            String order = SqlDownloadEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();
            PreparedStatement result;

            StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append("downloads");
            if (selection != null) {
                requestBuilder.append(" WHERE ").append(selection);
            }
            if (order != null) {
                requestBuilder.append(" ORDER BY ").append(order);
            }
            requestBuilder.append(" LIMIT " + DEFAULT_RESULT_LIMIT);
            String request = requestBuilder.toString();
            logger.info("QUERY: {}", request);

            PreparedStatement stmt1 = null;
            try {
                stmt1 = conn.prepareStatement(request);
                int index = 0;
                if (selectionArgs != null) {
                    for (String value : selectionArgs) {
                        stmt1.setString(++index, value);
                    }
                }
                result = stmt1;
            } catch (SQLException e) {
                e.printStackTrace();
                throw e;
            }
            stmt = result;
            rset = stmt.executeQuery();
            ArrayList<DownloadEntity> downloads = new ArrayList<DownloadEntity>();
            while (rset.next()) {
                downloads.add(SqlDownloadEntity.getDescriptor(rset));
            }
            return downloads;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try {
                if (rset != null) rset.close();
            } catch (Exception e) {
            }
            try {
                if (stmt != null) stmt.close();
            } catch (Exception e) {
            }
            try {
                if (conn != null) conn.close();
            } catch (Exception e) {
            }
        }
    }

    @Override
    public ArrayList<PurchaseEntity> getPurchases(String homeStoreId, long currPageHash) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = "homeStoreId=? AND ";
            String[] selectionArgs;
            if (currPageHash >= 0) {
                selection += "currPageHash=?";
                selectionArgs = new String[]{homeStoreId, String.valueOf(currPageHash)};
            } else {
                selection += "currPageHash = (SELECT currPageHash FROM downloads WHERE homeStoreId=? ORDER BY id DESC LIMIT 1)";
                selectionArgs = new String[]{homeStoreId, homeStoreId};
            }
            String order = SqlDownloadEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();
            PreparedStatement result;

            StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append("purchases");
            if (selection != null) {
                requestBuilder.append(" WHERE ").append(selection);
            }
            if (order != null) {
                requestBuilder.append(" ORDER BY ").append(order);
            }
            requestBuilder.append(" LIMIT " + DEFAULT_RESULT_LIMIT);
            String request = requestBuilder.toString();
            logger.info("QUERY: {}", request);

            PreparedStatement stmt1 = null;
            try {
                stmt1 = conn.prepareStatement(request);
                int index = 0;
                if (selectionArgs != null) {
                    for (String value : selectionArgs) {
                        stmt1.setString(++index, value);
                    }
                }
                result = stmt1;
            } catch (SQLException e) {
                e.printStackTrace();
                throw e;
            }
            stmt = result;
            rset = stmt.executeQuery();
            ArrayList<PurchaseEntity> purchases = new ArrayList<PurchaseEntity>();
            while (rset.next()) {
                purchases.add(SqlPurchaseEntity.getDescriptor(rset));
            }
            return purchases;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try {
                if (rset != null) rset.close();
            } catch (Exception e) {
            }
            try {
                if (stmt != null) stmt.close();
            } catch (Exception e) {
            }
            try {
                if (conn != null) conn.close();
            } catch (Exception e) {
            }
        }
    }

    @Override
    public ArrayList<ReviewEntity> getReviews(String homeStoreId, long currPageHash) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = "homeStoreId=? AND ";
            String[] selectionArgs;
            if (currPageHash >= 0) {
                selection += "currPageHash=?";
                selectionArgs = new String[]{homeStoreId, String.valueOf(currPageHash)};
            } else {
                selection += "currPageHash = (SELECT currPageHash FROM downloads WHERE homeStoreId=? ORDER BY id DESC LIMIT 1)";
                selectionArgs = new String[]{homeStoreId, homeStoreId};
            }
            String order = SqlDownloadEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();
            PreparedStatement result;

            StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append("reviews");
            if (selection != null) {
                requestBuilder.append(" WHERE ").append(selection);
            }
            if (order != null) {
                requestBuilder.append(" ORDER BY ").append(order);
            }
            requestBuilder.append(" LIMIT " + DEFAULT_RESULT_LIMIT);
            String request = requestBuilder.toString();
            logger.info("QUERY: {}", request);

            PreparedStatement stmt1 = null;
            try {
                stmt1 = conn.prepareStatement(request);
                int index = 0;
                if (selectionArgs != null) {
                    for (String value : selectionArgs) {
                        stmt1.setString(++index, value);
                    }
                }
                result = stmt1;
            } catch (SQLException e) {
                e.printStackTrace();
                throw e;
            }
            stmt = result;
            rset = stmt.executeQuery();
            ArrayList<ReviewEntity> reviews = new ArrayList<ReviewEntity>();
            while (rset.next()) {
                reviews.add(SqlReviewEntity.getDescriptor(rset));
            }
            return reviews;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try {
                if (rset != null) rset.close();
            } catch (Exception e) {
            }
            try {
                if (stmt != null) stmt.close();
            } catch (Exception e) {
            }
            try {
                if (conn != null) conn.close();
            } catch (Exception e) {
            }
        }
    }

    /**
     * insert new record to table with paging (add page hashes to insert)
     */
    private void insertWithHashes(Connection connection, String tableName, BaseEntity entity, int limit) throws SQLException {
        insertWithHashes(connection, tableName, null, entity, limit);
    }

    /**
     * insert new record to table with paging (add page hashes to insert)
     */
    private void insertWithHashes(Connection connection, String tableName, String packageName, BaseEntity entity, int limit) throws SQLException {
        Pair<Integer, Integer> pageHashes = getPageHashes(connection, tableName, packageName, limit);
        entity.setPrevPageHash(pageHashes.fst);
        entity.setCurrPageHash(pageHashes.snd);
        saveEntity(entity);
    }


    /**
     * @param connection
     * @param tableName
     * @param packageName is used in downloads, purchases, reviews
     * @param limit       number of the records per one page
     * @return pair <currPageHash, prevPageHash>
     * @throws java.sql.SQLException
     */
    private static Pair<Integer, Integer> getPageHashes(Connection connection, String tableName, String packageName, int limit) throws SQLException {
        String selection = "SELECT count(currPageHash) as cunt, currPageHash as chash, prevPageHash as phash FROM " + tableName + " WHERE currPageHash = (SELECT currPageHash FROM " + tableName;
        if (packageName != null) {
            selection += " WHERE package=?";
        }
        selection += " ORDER BY id DESC LIMIT 1)";

        PreparedStatement stmt = connection.prepareStatement(selection);
        if (packageName != null) {
            stmt.setString(0, packageName);
        }
        ResultSet rs = stmt.executeQuery();
        int count = 0;
        int chash = 0, phash = 0;
        if (rs.next()) {
            count = rs.getInt(1);
            chash = rs.getInt(2);
            phash = rs.getInt(3);
        }
        if (count >= limit) {
            phash = chash;
            chash += 1;
        }
        rs.close();
        stmt.close();
        return new Pair<Integer, Integer>(chash, phash);
    }


    public void saveEntity(BaseEntity entity) {
        Session session = getSession();
        session.beginTransaction();
        session.save(entity);
        session.getTransaction().commit();
        session.close();
    }

}
