(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :phong)
(learnopengl::phong)
