
import { CodeShareLink } from '@/types/compiler';

const CODE_SHARE_KEY = 'codeer_shared_links';

// Generate a random ID
const generateId = () => {
  return Math.random().toString(36).substring(2, 15) + 
         Math.random().toString(36).substring(2, 15);
};

// Get shared links from localStorage
const getSharedLinks = (): CodeShareLink[] => {
  const linksJson = localStorage.getItem(CODE_SHARE_KEY);
  if (!linksJson) return [];
  
  try {
    const links = JSON.parse(linksJson);
    return links.map((link: any) => ({
      ...link,
      expiresAt: new Date(link.expiresAt)
    }));
  } catch (error) {
    console.error('Failed to parse shared links', error);
    return [];
  }
};

// Save shared links to localStorage
const saveSharedLinks = (links: CodeShareLink[]) => {
  localStorage.setItem(CODE_SHARE_KEY, JSON.stringify(links));
};

// Create a new shared link
export const createShareLink = (code: string, language: string): string => {
  const links = getSharedLinks();
  
  // Clean up expired links
  const now = new Date();
  const validLinks = links.filter(link => new Date(link.expiresAt) > now);
  
  // Create expiry date (48 hours from now)
  const expiresAt = new Date();
  expiresAt.setHours(expiresAt.getHours() + 48);
  
  // Create new link
  const newLink: CodeShareLink = {
    id: generateId(),
    code,
    language,
    expiresAt,
  };
  
  // Add to valid links and save
  validLinks.push(newLink);
  saveSharedLinks(validLinks);
  
  // Return share URL
  const baseUrl = window.location.origin;
  return `${baseUrl}/compiler?share=${newLink.id}`;
};

// Get a shared code by ID
export const getSharedCode = (id: string): { code: string, language: string } | null => {
  const links = getSharedLinks();
  const now = new Date();
  
  // Find the link
  const link = links.find(link => link.id === id);
  
  // If found and not expired, return it
  if (link && new Date(link.expiresAt) > now) {
    return {
      code: link.code,
      language: link.language
    };
  }
  
  return null;
};

// Clean up expired links
export const cleanupExpiredLinks = () => {
  const links = getSharedLinks();
  const now = new Date();
  const validLinks = links.filter(link => new Date(link.expiresAt) > now);
  
  if (validLinks.length !== links.length) {
    saveSharedLinks(validLinks);
  }
};
