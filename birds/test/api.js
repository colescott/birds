process.env.NODE_ENV = 'test';

const request = require('supertest');
const app = require("../app.js");

const testUser = {email:'test@team4159.org', password: 'password', firstname: 'Test', lastname: 'Account', teamnumber: 4159};
const testUserNoPass = {email: testUser.email, firstname: testUser.firstname, lastname: testUser.lastname, teamnumber: testUser.teamnumber};;

var testUserWithId;
var loginToken;

describe('POST /api/v1/users', () => {
    it('respond with json', (done) => {
        request(app)
        .post('/api/v1/users')
        .set('Accept', 'application/json')
        .expect('Content-Type', /json/)
        .expect(200, done);
    });
    it('created and returned new user', (done) => {
        request(app)
        .post('/api/v1/users')
        .set('Accept', 'application/json')
        .send(testUser)
        .expect('Content-Type', /json/)
        .expect(function(res) {
            if(res.body.name)
                throw new Error('User already exists in database');
            delete res.body.data.user.id;
        })
        .expect(200, {
            data: {
                user: testUserNoPass
            }
        }, done);
    });
});

describe('GET /api/v1/users', () => {
    it('respond with json', (done) => {
        request(app)
        .get('/api/v1/users')
        .set('Accept', 'application/json')
        .expect('Content-Type', /json/)
        .expect(200, done);
    });
    it('conatins test user', (done) => {
        request(app)
        .get('/api/v1/users')
        .set('Accept', 'application/json')
        .expect(function(res) {
            var found = false;
            res.body.data.users.forEach( (user) => {
                if(user.email == testUser.email)
                {
                    testUserWithId = clone(user);
                    found = true;
                }
            });
            if(!found)
                throw new Error('user does not exist!');
        })
        .end(done);
    });
});

describe('POST /api/v1/auth/login', () => {
    it('respond with json', (done) => {
        request(app)
        .post('/api/v1/auth/login')
        .set('Accept', 'application/json')
        .send(testUser)
        .expect('Content-Type', /json/)
        .expect(200, done);
    });
    it('responds with user', (done) => {
        request(app)
        .post('/api/v1/auth/login')
        .set('Accept', 'application/json')
        .send(testUser)
        .expect(function(res) {
            if(res.body.data.user.id != testUserWithId.id)
                throw new Error('user not in response')
        })
        .end(done);
    });
    it('responds with token', (done) => {
        request(app)
        .post('/api/v1/auth/login')
        .set('Accept', 'application/json')
        .send(testUser)
        .expect(function(res) {
            if(!res.body.data.token)
                throw new Error('Login token not set')
            loginToken = res.body.data.token;
        })
        .end(done);
    });
});

const clone = (obj) => {
    if (null == obj || "object" != typeof obj) return obj;
    var copy = obj.constructor();
    for (var attr in obj) {
        if (obj.hasOwnProperty(attr)) copy[attr] = obj[attr];
    }
    return copy;
}
