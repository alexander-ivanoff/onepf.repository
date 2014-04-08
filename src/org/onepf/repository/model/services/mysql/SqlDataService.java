package org.onepf.repository.model.services.mysql;

import org.apache.commons.dbcp.ConnectionFactory;
import org.apache.commons.dbcp.DriverManagerConnectionFactory;
import org.apache.commons.dbcp.PoolableConnectionFactory;
import org.apache.commons.dbcp.PoolingDataSource;
import org.apache.commons.pool.ObjectPool;
import org.apache.commons.pool.impl.GenericObjectPool;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.Pair;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.PurchaseDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ReviewDescriptor;
import org.onepf.repository.appstorelooter.LastUpdateDescriptor;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.mysql.entities.*;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlDataService implements DataService {

    // TODO refactoring: move method in different requests (Maybe Entities), here should be only generic requests

    private static final int PAGE_LIMIT_APPLICATIONS = 3;
    private static final int PAGE_LIMIT_OTHER = 3;

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
        PoolableConnectionFactory poolableConnectionFactory = new PoolableConnectionFactory (connectionFactory,connectionPool,null,null,false,true);
        PoolingDataSource dataSource = new PoolingDataSource(connectionPool);
        return dataSource;
    }

    @Override
    public void store(ApplicationDescriptor applicationDescriptor) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        try {
            SqlAppEntity appEntity = new SqlAppEntity()
                    .withPackageName(applicationDescriptor.packageName)
                    .withLastUpdate(applicationDescriptor.lastUpdated)
                    .withDevelopersContact(applicationDescriptor.developerContact)
                    .withAppstore(applicationDescriptor.appstoreId)
                    .withAppdf(applicationDescriptor.appdfLink)
                    .withDescription(applicationDescriptor.descriptionLink)
                    .withHash(applicationDescriptor.appdfHash);
            conn = dbDataSource.getConnection();
            stmt = insertWithHashes(conn, SqlAppEntity.TABLE_NAME, appEntity, PAGE_LIMIT_APPLICATIONS);
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
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
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public void addDownload(DownloadDescriptor downloadDescriptor) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        try {
            SqlDownloadEntity entity = new SqlDownloadEntity()
                    .withLastUpdate(downloadDescriptor.lastUpdate)
                    .withBuild(downloadDescriptor.build)
                    .withPackageName(downloadDescriptor.packageName)
                    .withCountry(downloadDescriptor.country)
                    .withDateTime(downloadDescriptor.dateTime)
                    .withDeviceModel(downloadDescriptor.deviceModel)
                    .withDeviceName(downloadDescriptor.deviceName)
                    .withIsUpdate(downloadDescriptor.isUpdate);

            conn = dbDataSource.getConnection();
            stmt = insertWithHashes(conn, SqlDownloadEntity.TABLE_NAME, downloadDescriptor.packageName, entity, PAGE_LIMIT_OTHER);
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }

    }

    @Override
    public List<ApplicationDescriptor> getApplicationsLog() throws DataException{
        return getApplicationsLog(null, -1);
    }


    @Override
    public List<ApplicationDescriptor> getApplicationsLog(String packageName, int currPageHash) throws DataException{
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = null;
            String[] selectionArgs = null;
            conn = dbDataSource.getConnection();
            if (packageName != null) {
                selection = SqlAppEntity.FIELD_PACKAGE_NAME + "=?";
                selectionArgs = new String[] {packageName};
            } else {
                if (currPageHash >= 0) {
                    selection = SqlAppEntity.FIELD_CURR_PAGE_HASH + "=?";
                    selectionArgs = new String[] {String.valueOf(currPageHash)};
                } else {
                    selection = SqlAppEntity.FIELD_CURR_PAGE_HASH + " = (SELECT " + SqlAppEntity.FIELD_CURR_PAGE_HASH +
                            " FROM " + SqlAppEntity.TABLE_NAME + " ORDER BY " + SqlAppEntity.FIELD_ID + " DESC LIMIT 1)";
                    selectionArgs = null;
                }
            }
            String orderBy = SqlAppEntity.FIELD_ID + " DESC";
            stmt = query(conn, SqlAppEntity.TABLE_NAME, selection, selectionArgs, orderBy, DEFAULT_RESULT_LIMIT);
            rset = stmt.executeQuery();
            ArrayList<ApplicationDescriptor> apps = new ArrayList<ApplicationDescriptor>();
            while (rset.next()) {
                apps.add(SqlAppEntity.getDescriptor(rset));
            }
            return apps;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public List<ApplicationDescriptor> getApplicationByHash(String packageName, String hash) throws DataException{
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = SqlAppEntity.FIELD_PACKAGE_NAME + "=? AND " + SqlAppEntity.FIELD_HASH + "=?";
            String[] selectionArgs = new String[] {packageName, hash};
            String orderBy = SqlAppEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();

            stmt = query(conn, SqlAppEntity.TABLE_NAME, selection, selectionArgs, orderBy, 1);
            rset = stmt.executeQuery();
            ArrayList<ApplicationDescriptor> apps = new ArrayList<ApplicationDescriptor>();
            while (rset.next()) {
                apps.add(SqlAppEntity.getDescriptor(rset));
            }
            return apps;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }




    @Override
    public Map<String, AppstoreDescriptor> getAppstores() throws DataException{
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            conn = dbDataSource.getConnection();
            stmt = query(conn, SqlAppstoreEntity.TABLE_NAME, null, null, null, DEFAULT_RESULT_LIMIT);
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
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public List<LastUpdateDescriptor> getLastUpdate(String appstoreId) throws DataException{
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            conn = dbDataSource.getConnection();
            String selection = SqlLastUpdateEntity.FIELD_APPSTORE_ID + "=?";
            String[] selectionArgs = new String[] {appstoreId};
            stmt = query(conn, SqlLastUpdateEntity.TABLE_NAME, selection, selectionArgs, null, DEFAULT_RESULT_LIMIT);
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
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public ArrayList<DownloadDescriptor> getDownloads(String packageName, long currPageHash) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = SqlDownloadEntity.FIELD_PACKAGE_NAME + "=? AND ";
            String[] selectionArgs;
            if (currPageHash >= 0) {
                selection += "currPageHash=?";
                selectionArgs = new String[] {packageName, String.valueOf(currPageHash)};
            } else {
                selection += "currPageHash = (SELECT currPageHash FROM downloads WHERE " + SqlDownloadEntity.FIELD_PACKAGE_NAME + "=? ORDER BY id DESC LIMIT 1)";
                selectionArgs = new String[] {packageName, packageName};
            }
            String order = SqlDownloadEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();
            stmt = query(conn, "downloads", selection, selectionArgs, order, DEFAULT_RESULT_LIMIT);
            rset = stmt.executeQuery();
            ArrayList<DownloadDescriptor> downloads = new ArrayList<DownloadDescriptor>();
            while (rset.next()) {
                downloads.add(SqlDownloadEntity.getDescriptor(rset));
            }
            return downloads;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public ArrayList<PurchaseDescriptor> getPurchases(String packageName, long currPageHash) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = SqlPurchaseEntity.FIELD_PACKAGE_NAME + "=? AND ";
            String[] selectionArgs;
            if (currPageHash >= 0) {
                selection += "currPageHash=?";
                selectionArgs = new String[] {packageName, String.valueOf(currPageHash)};
            } else {
                selection += "currPageHash = (SELECT currPageHash FROM purchases WHERE " + SqlPurchaseEntity.FIELD_PACKAGE_NAME + "=? ORDER BY id DESC LIMIT 1)";
                selectionArgs = new String[] {packageName, packageName};
            }
            String order = SqlDownloadEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();
            stmt = query(conn, "purchases", selection, selectionArgs, order, DEFAULT_RESULT_LIMIT);
            rset = stmt.executeQuery();
            ArrayList<PurchaseDescriptor> purchases = new ArrayList<PurchaseDescriptor>();
            while (rset.next()) {
                purchases.add(SqlPurchaseEntity.getDescriptor(rset));
            }
            return purchases;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public ArrayList<ReviewDescriptor> getReviews(String packageName, int currPageHash) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = SqlReviewEntity.FIELD_PACKAGE_NAME + "=? AND ";
            String[] selectionArgs;
            if (currPageHash >= 0) {
                selection += "currPageHash=?";
                selectionArgs = new String[] {packageName, String.valueOf(currPageHash)};
            } else {
                selection += "currPageHash = (SELECT currPageHash FROM reviews WHERE " + SqlReviewEntity.FIELD_PACKAGE_NAME + "=? ORDER BY id DESC LIMIT 1)";
                selectionArgs = new String[] {packageName, packageName};
            }
            String order = SqlDownloadEntity.FIELD_ID + " DESC";
            conn = dbDataSource.getConnection();
            stmt = query(conn, "reviews", selection, selectionArgs, order, DEFAULT_RESULT_LIMIT);
            rset = stmt.executeQuery();
            ArrayList<ReviewDescriptor> reviews = new ArrayList<ReviewDescriptor>();
            while (rset.next()) {
                reviews.add(SqlReviewEntity.getDescriptor(rset));
            }
            return reviews;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }


    // SELECT count(currPageHash), currPageHash, prevPageHash INTO @cunt, @chash, @phash FROM onepf_repository.applications WHERE  currPageHash = (SELECT currPageHash FROM applications ORDER BY id DESC LIMIT 1);
    // INSERT INTO applications (appstoreId, currPageHash, prevPageHash, appdfLink, descrLink) VALUES ('com.appstore.test1', IF (@cunt>=3, @chash+1, @chash), IF (@cunt>=3, @chash, @phash), 'aa', 'bb');

    private static PreparedStatement insertWithHashes(Connection connection, String tableName, SqlDBEntity dbEntity, int limit) throws SQLException {
        return insertWithHashes(connection, tableName, null,  dbEntity, limit);
    }

    private static PreparedStatement insertWithHashes(Connection connection, String tableName, String packageName, SqlDBEntity dbEntity, int limit) throws SQLException {

        Pair<Integer, Integer> pageHashes = getPageHashes(connection, tableName, packageName, limit);

        StringBuilder columnsBuilder = new StringBuilder().append("(");
        StringBuilder valuesBuilder = new StringBuilder().append("(");
        Map<String, String> item = dbEntity.getItem();
        for (String entry : item.keySet()) {
            columnsBuilder.append(entry).append(',');
            valuesBuilder.append('?').append(',');

        }

        columnsBuilder.append("currPageHash").append(',');
        columnsBuilder.append("prevPageHash").append(')');
        valuesBuilder.append("?").append(',');
        valuesBuilder.append("?").append(')');

        String request = "INSERT INTO " + tableName + " " + columnsBuilder.toString() + " VALUES " + valuesBuilder.toString() + ";";
        logger.info("INSERT: {}", request);

        PreparedStatement stmt = connection.prepareStatement(request);
        Collection<String> values = item.values();
        int index = 0;
        for (String value: values) {
            stmt.setString(++index, value);
        }
        stmt.setInt(++index, pageHashes.fst);
        stmt.setInt(++index, pageHashes.snd);
            return  stmt;

    }


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
        for (String value: values) {
            stmt.setString(++index, value);
        }
        return  stmt;

    }

    private static PreparedStatement query(Connection connection,  String tableName, String selection, String[] selectionArgs, String order, int limit) throws SQLException {

        StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append(tableName);
        if (selection != null) {
            requestBuilder.append(" WHERE ").append(selection);
        };
        if (order != null) {
            requestBuilder.append(" ORDER BY ").append(order);
        }
        requestBuilder.append(" LIMIT " + limit);
        String request = requestBuilder.toString();
        logger.info("QUERY: {}", request);

        PreparedStatement stmt = null;
        try {
            stmt = connection.prepareStatement(request);
            int index = 0;
            if (selectionArgs != null) {
                for (String value: selectionArgs) {
                    stmt.setString(++index, value);
                }
            }
            return stmt;
        } catch (SQLException e) {
            e.printStackTrace();
            throw e;
        }
    }


    /*
    **
    * returns pair <currPageHash, prevPageHash>, packagename is used in downloads, purchases, reviews
    **
    */
    private static Pair<Integer, Integer> getPageHashes(Connection connection,  String tableName, String packageName,  int limit) throws SQLException {

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
        return  new Pair<Integer, Integer>(chash, phash);
    }

}
