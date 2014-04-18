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
import org.onepf.repository.appstorelooter.LastUpdateDescriptor;
import org.onepf.repository.model.auth.AppstoreDescriptor;
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
    public void saveLastUpdate(LastUpdateDescriptor lastUpdateDescriptor) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        try {
            SqlLastUpdateEntity appEntity = new SqlLastUpdateEntity()
                    .withAppstoreId(lastUpdateDescriptor.appstoreId)
                    .withDateTime(lastUpdateDescriptor.lastResponseDatetime)
                    .withHash(lastUpdateDescriptor.lastResponseHash)
                    .withOffset(lastUpdateDescriptor.prevOffset);

            conn = dbDataSource.getConnection();
            stmt = insert(conn, SqlLastUpdateEntity.TABLE_NAME, appEntity);
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
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
    public  List<ApplicationEntity> getApplicationsLog(String packageName, int currPageHash) throws DataException {
        Configuration configuration = new Configuration();
        configuration.configure("/resources/hibernate.cfg.xml");
        StandardServiceRegistry serviceRegistry = new StandardServiceRegistryBuilder().applySettings(
                configuration.getProperties()).build();
        SessionFactory sessionFactory = configuration
                .buildSessionFactory(serviceRegistry);
        Session session = sessionFactory.openSession();

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
                String hqlSelection = "FROM ApplicationEntity ORDER BY id DESC ";
                Query query = session.createQuery(hqlSelection);
                List list = query.list();
                if (!list.isEmpty()) {
                    pageHash = ((ApplicationEntity) list.get(0)).getCurrPageHash();
                } else {
                    pageHash = 0;
                }
                params.add(new Pair<String, Object>("currPageHashParam", pageHash));
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
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = SqlAppEntity.FIELD_PACKAGE_NAME + "=? AND " + SqlAppEntity.FIELD_HASH + "=?";
            String[] selectionArgs = new String[]{packageName, hash};
            String orderBy = SqlAppEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();

            PreparedStatement result;

            StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append(SqlAppEntity.TABLE_NAME);
            if (selection != null) {
                requestBuilder.append(" WHERE ").append(selection);
            }
            if (orderBy != null) {
                requestBuilder.append(" ORDER BY ").append(orderBy);
            }
            requestBuilder.append(" LIMIT " + 1);
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
            ArrayList<ApplicationEntity> apps = new ArrayList<ApplicationEntity>();
            while (rset.next()) {
                apps.add(SqlAppEntity.getDescriptor(rset));
            }
            return apps;
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
    public Map<String, AppstoreDescriptor> getAppstores() throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            conn = dbDataSource.getConnection();
            PreparedStatement result;

            StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append(SqlAppstoreEntity.TABLE_NAME);
            if (null != null) {
                requestBuilder.append(" WHERE ").append((String) null);
            }
            if (null != null) {
                requestBuilder.append(" ORDER BY ").append((String) null);
            }
            requestBuilder.append(" LIMIT " + DEFAULT_RESULT_LIMIT);
            String request = requestBuilder.toString();
            logger.info("QUERY: {}", request);

            PreparedStatement stmt1 = null;
            try {
                stmt1 = conn.prepareStatement(request);
                int index = 0;
                if (null != null) {
                    for (String value : (String[]) null) {
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
            Map<String, AppstoreDescriptor> apps = new HashMap<String, AppstoreDescriptor>();
            AppstoreDescriptor appstore = null;
            while (rset.next()) {
                appstore = SqlAppstoreEntity.getDescriptor(rset);
                apps.put(appstore.repositoryAccessToken, appstore);
            }
            return apps;
        } catch (SQLException e) {
            e.printStackTrace();
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
    public List<LastUpdateDescriptor> getLastUpdate(String appstoreId) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            conn = dbDataSource.getConnection();
            String selection = SqlLastUpdateEntity.FIELD_APPSTORE_ID + "=?";
            String[] selectionArgs = new String[]{appstoreId};
            PreparedStatement result;

            StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append(SqlLastUpdateEntity.TABLE_NAME);
            if (selection != null) {
                requestBuilder.append(" WHERE ").append(selection);
            }
            if (null != null) {
                requestBuilder.append(" ORDER BY ").append((String) null);
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
            List<LastUpdateDescriptor> updates = new ArrayList<LastUpdateDescriptor>();
            while (rset.next()) {
                updates.add(SqlLastUpdateEntity.getDescriptor(rset));
            }
            return updates;
        } catch (SQLException e) {
            e.printStackTrace();
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
    private static void insertWithHashes(Connection connection, String tableName, BaseEntity entity, int limit) throws SQLException {
        insertWithHashes(connection, tableName, null, entity, limit);
    }

    /**
     * insert new record to table with paging (add page hashes to insert)
     */
    private static void insertWithHashes(Connection connection, String tableName, String packageName, BaseEntity entity, int limit) throws SQLException {
        Pair<Integer, Integer> pageHashes = getPageHashes(connection, tableName, packageName, limit);
        entity.setPrevPageHash(pageHashes.fst);
        entity.setCurrPageHash(pageHashes.snd);
        saveEntity(entity);
    }

    /**
     * insert or replace record to table without paging
     */
    private static PreparedStatement insert(Connection connection, String tableName, SqlDBEntity dbEntity) throws SQLException {

        StringBuilder columnsBuilder = new StringBuilder().append("(");
        StringBuilder valuesBuilder = new StringBuilder().append("(");
        Map<String, String> item = dbEntity.getItem();
        for (String entry : item.keySet()) {
            columnsBuilder.append(entry).append(',');
            valuesBuilder.append('?').append(',');

        }
        int pos = columnsBuilder.length();
        columnsBuilder.replace(pos - 1, pos, ")");
        pos = valuesBuilder.length();
        valuesBuilder.replace(pos - 1, pos, ")");

        String request = "REPLACE INTO " + tableName + " " + columnsBuilder.toString() + " VALUES " + valuesBuilder.toString() + ";";

        PreparedStatement stmt = connection.prepareStatement(request);
        Collection<String> values = item.values();
        int index = 0;
        for (String value : values) {
            stmt.setString(++index, value);
        }
        return stmt;

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


    public static void saveEntity(BaseEntity entity) {
        Configuration configuration = new Configuration();
        configuration.configure("/resources/hibernate.cfg.xml");
        StandardServiceRegistry serviceRegistry = new StandardServiceRegistryBuilder().applySettings(
                configuration.getProperties()).build();
        SessionFactory sessionFactory = configuration
                .buildSessionFactory(serviceRegistry);
        Session session = sessionFactory.openSession();
        session.beginTransaction();
        session.save(entity);
        session.getTransaction().commit();
        session.close();
    }

}
