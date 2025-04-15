
import { Link } from "react-router-dom";

const Index = () => {
  return (
    <div className="container mx-auto p-4 text-center space-y-6">
      <h1 className="text-4xl font-bold mb-4">WebCompiler</h1>
      <p className="text-xl text-gray-600">Online Code Editor powered by Judge0</p>
      <div className="flex justify-center space-x-4">
        <Link 
          to="/compiler" 
          className="bg-primary text-primary-foreground px-4 py-2 rounded hover:bg-primary/90 transition-colors"
        >
          Open Code Editor
        </Link>
        <a 
          href="https://github.com/sriox/webcompiler" 
          target="_blank" 
          rel="noopener noreferrer"
          className="border border-primary text-primary px-4 py-2 rounded hover:bg-primary/10 transition-colors"
        >
          GitHub Repository
        </a>
      </div>
    </div>
  );
};

export default Index;
