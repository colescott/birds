const request = require('supertest');
const app = require("../app.js");

it("should run this test", () => {});

describe('GET /api/v1/users', function() {
  it('respond with json', function(done) {
    request(app)
      .get('/api/v1/users')
      .set('Accept', 'application/json')
      .expect('Content-Type', /json/)
      .expect(200, done);
  });
});
